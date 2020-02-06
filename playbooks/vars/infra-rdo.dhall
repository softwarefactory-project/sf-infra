let Package = ../../conf/common.dhall

let Flavors = Package.Flavors

let Image = Package.Image

let Rule = Package.Rule

let Server = Package.Server

let mkNetwork = Package.mkNetwork

let mkRouter = Package.mkRouter

let mkSubnet = Package.mkSubnet

let fqdn = "rdoproject.org"

let rdo_network = { name = "private", network_prefix = "192.168.240" }

let backward-compat-name = { name = "default-router" }

let security_groups =
        Package.SecurityGroups
      # [ { name = "afs"
          , rules =
            [ Rule::{ port = +8080 }
            , Rule::{ port = +8081 }
            , Rule::{ port = +8082 }
            ]
          }
        , { name = "monitoring"
          , rules =
            [ Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ bridge_public_ip }}/32"
              }
            , Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "rdo-trunk", rules = [ Rule::{ port = +3300 } ] }
        , { name = "registry"
          , rules =
            [ Rule::{ port = +53 }
            , Rule::{ port = +2379 }
            , Rule::{ port = +2380 }
            , Rule::{ port = +4001 }
            , Rule::{ port = +5000 }
            , Rule::{ port = +8443 }
            , Rule::{ port = +9090 }
            , Rule::{ port = +10250 }
            , Rule::{ port = +4789, protocol = Some "udp" }
            , Rule::{ port = +8053, protocol = Some "udp" }
            ]
          }
        ]

let images =
      [ Image::{
        , name = "centos-7-1907"
        , url =
            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
        , checksum =
            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
        }
      , Image::{
        , name = "fedora-30-1.2"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/30/Cloud/x86_64/images/Fedora-Cloud-Base-30-1.2.x86_64.qcow2"
        , checksum =
            "72b6ae7b4ed09a4dccd6e966e1b3ac69bd97da419de9760b410e837ba00b4e26"
        }
      , Image::{
        , name = "centos-8.0-1905"
        , url =
            "https://jpena.fedorapeople.org/CentOS-8-GenericCloud-8.0.1905-22.x86_64.qcow2"
        , checksum =
            "c86c119665866695a700a4ab523c774c64ed7e0dd9e6e89f5f032e0f03148a47"
        }
      , Image::{
        , name = "centos-8.1-1911"
        , url =
            "https://cloud.centos.org/centos/8/x86_64/images/CentOS-8-GenericCloud-8.1.1911-20200113.3.x86_64.qcow2"
        , checksum =
            "e2cf1081645b1089f574918fb808b32d247169ec4ec1a13bca9e14a74df6530e"
        }
      ]

let volumes =
      [ { display_name = "registry-data"
        , size = 1024
        , server = "registry-vexxhost.rdoproject.org"
        , device = "/dev/vdb"
        }
      , { display_name = "centos8-rpm-packaging-swap"
        , size = 8
        , server = "centos8-rpm-packaging-ci.rdoproject.org"
        , device = "/dev/vdb"
        }
      , { display_name = "logs-data"
        , size = 1000
        , server = "logserver.rdoproject.org"
        , device = "/dev/vdb"
        }
      , { display_name = "images-data"
        , size = 500
        , server = "images-vexxhost.rdoproject.org"
        , device = "/dev/vdb"
        }
      , { display_name = "trunk-centos7-swap"
        , size = 8
        , server = "trunk-centos7.rdoproject.org"
        , device = "/dev/vdb"
        }
      ]

let servers =
      [ Server::{
        , name = "mirror.regionone.vexxhost"
        , floating_ip = "yes"
        , security_groups = [ "afs", "common", "web", "monitoring" ]
        , volume_size = Some 200
        }
      , Server::{
        , name = "centos8-rpm-packaging-ci"
        , image = "centos-8.0-1905"
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring", "rdo-trunk" ]
        , volume_size = Some 100
        , volumes = Some [ "centos8-rpm-packaging-swap" ]
        }
      , Server::{
        , name = "rpm-packaging-ci"
        , flavor = "ci.m1.large"
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring", "rdo-trunk" ]
        , volume_size = Some 100
        }
      , Server::{
        , name = "fedora-rpm-packaging-ci"
        , image = "fedora-30-1.2"
        , flavor = "ci.m1.large"
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring", "rdo-trunk" ]
        , volume_size = Some 100
        }
      , Server::{
        , name = "registry-vexxhost"
        , flavor = Flavors.`4cpus_16gig`
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring", "registry" ]
        , volume_size = Some 200
        , volumes = Some [ "registry-data" ]
        }
      , Server::{
        , name = "trunk-centos8"
        , image = "centos-8.0-1905"
        , flavor = Flavors.`4cpus_16gig`
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring", "rdo-trunk" ]
        , volume_size = Some 512
        }
      , Server::{
        , name = "trunk-centos7"
        , flavor = Flavors.`4cpus_16gig`
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring", "rdo-trunk" ]
        , volume_size = Some 512
        }
      , Server::{
        , name = "install-server"
        , flavor = Flavors.`1cpu_4gig`
        , floating_ip = "yes"
        , security_groups = [ "common", "monitoring" ]
        , volume_size = Some 40
        }
      , Server::{
        , name = "logserver"
        , floating_ip = "yes"
        , security_groups = [ "common", "monitoring", "web" ]
        , volume_size = Some 10
        , volumes = Some [ "logs-data" ]
        }
      , Server::{
        , name = "images-vexxhost"
        , image = "centos-8.1-1911"
        , flavor = Flavors.`1cpu_4gig`
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring" ]
        , volume_size = Some 50
        , volumes = Some [ "images-data" ]
        }
      , Server::{
        , name = "www"
        , image = "centos-8.1-1911"
        , flavor = Flavors.`1cpu_4gig`
        , floating_ip = "yes"
        , security_groups = [ "common", "web", "monitoring" ]
        , volume_size = Some 10
        }
      ]

in  { servers = Package.setFqdn fqdn servers
    , networks = [ mkNetwork rdo_network.name ]
    , subnets = [ mkSubnet rdo_network.name rdo_network.network_prefix ]
    , routers =
      [     mkRouter rdo_network.name rdo_network.network_prefix
        //  backward-compat-name
      ]
    , keypairs =
      [ { name = "sf-infra-key", public_key = Package.sfInfraKeypair } ]
    , images = images
    , image_cache_dir = "{{ ansible_user_dir }}/image_cache"
    , volumes = volumes
    , security_groups = security_groups
    }
