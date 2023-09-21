// Copyright (C) 2023 Red Hat
// SPDX-License-Identifier: Apache-2.0
package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/gophercloud/gophercloud"
	"github.com/gophercloud/gophercloud/openstack/compute/v2/flavors"
	"github.com/gophercloud/gophercloud/openstack/compute/v2/servers"
	"github.com/gophercloud/utils/openstack/clientconfig"
)

type CloudMetric struct {
	client  *gophercloud.ServiceClient
	flavors map[string]flavors.Flavor
	disk    int
	ram     int
	vcpu    int
}

func newCloudMetric(cloud string) *CloudMetric {
	opts := new(clientconfig.ClientOpts)
	opts.Cloud = cloud

	client, err := clientconfig.NewServiceClient("compute", opts)
	if err != nil {
		panic(err)
	}

	listOpts := flavors.ListOpts{
		AccessType: flavors.PublicAccess,
	}

	allPages, err := flavors.ListDetail(client, listOpts).AllPages()
	if err != nil {
		panic(err)
	}

	allFlavors, err := flavors.ExtractFlavors(allPages)
	if err != nil {
		panic(err)
	}

	flavors := make(map[string]flavors.Flavor)
	for _, flavor := range allFlavors {
		flavors[flavor.ID] = flavor
	}
	disk := 0
	ram := 0
	vcpu := 0
	return &CloudMetric{client, flavors, disk, ram, vcpu}
}

func (cloudMetric *CloudMetric) GetOpenstackProjectUsage() {
	pager, err := servers.List(cloudMetric.client, nil).AllPages()
	if err != nil {
		fmt.Println(err)
	}

	servers, err := servers.ExtractServers(pager)
	if err != nil {
		fmt.Println(err)
	}

	cloudMetric.vcpu = 0
	cloudMetric.ram = 0
	cloudMetric.disk = 0
	for _, server := range servers {
		if server.Status == "ACTIVE" {
			if flavor, ok := cloudMetric.flavors[server.Flavor["id"].(string)]; ok {
				cloudMetric.vcpu += flavor.VCPUs
				cloudMetric.ram += flavor.RAM
				cloudMetric.disk += flavor.Disk
			} else {
				fmt.Printf("Unknown flavor: %s\n", server.Flavor)
			}
		}
	}
}

type countHandler struct {
	mu        sync.Mutex
	updatedAt time.Time
	clouds    map[string]*CloudMetric
}

func (countHandler *countHandler) ServeHTTP(w http.ResponseWriter, _r *http.Request) {
	countHandler.mu.Lock()
	defer countHandler.mu.Unlock()

	now := time.Now()
	if now.Sub(countHandler.updatedAt).Seconds() > 10 {
		countHandler.updatedAt = now
		for _, cloudMetric := range countHandler.clouds {
			cloudMetric.GetOpenstackProjectUsage()
		}
	}

	io.WriteString(w, "# HELP metric\n# TYPE metric counter\nmetric 3.0\n")
	io.WriteString(w, "# HELP cloud_ram_usage The RAM usage in bytes.\n# TYPE cloud_ram_usage gauge\n")
	for cloud, cloudMetric := range countHandler.clouds {
		fmt.Fprintf(w, "cloud_ram_usage{project=\"%s\"} %d\n", cloud, cloudMetric.ram)
	}
	io.WriteString(w, "# HELP cloud_vcpu_usage The VCPU usage in core.\n# TYPE cloud_vcpu_usage gauge\n")
	for cloud, cloudMetric := range countHandler.clouds {
		fmt.Fprintf(w, "cloud_vcpu_usage{project=\"%s\"} %d\n", cloud, cloudMetric.vcpu)
	}
	io.WriteString(w, "# HELP cloud_disk_usage The Disk usage in bytes.\n# TYPE cloud_disk_usage gauge\n")
	for cloud, cloudMetric := range countHandler.clouds {
		fmt.Fprintf(w, "cloud_disk_usage{project=\"%s\"} %d\n", cloud, cloudMetric.disk)
	}
}

func main() {
	handler := new(countHandler)

	// Setup registry
	var projects string
	flag.StringVar(&projects, "projects", "", "projects names eg 'projectA,projectB'")
	flag.Parse()
	if projects != "" {
		clouds := strings.Split(projects, ",")
		handler.clouds = make(map[string]*CloudMetric)
		for _, cloud := range clouds {
			fmt.Printf("Initializing cloud %s\n", cloud)
			handler.clouds[cloud] = newCloudMetric(cloud)
		}
	} else {
		fmt.Println("projects name should be provided")
		os.Exit(1)
	}

	// Setup http handler
	http.Handle("/metrics", handler)
	fmt.Printf("Listening to %s\n", ":8081")
	log.Fatal(http.ListenAndServe(":8081", nil))
}
