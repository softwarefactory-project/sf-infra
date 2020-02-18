let Infra = ../../conf/infra.dhall

let fqdn = "softwarefactory-project.io"

let default-security-groups = [ "common", "monitoring" ]

let sf_network = { name = "private", network_prefix = "192.168.242" }

let oci_network = { name = "oci-private", network_prefix = "192.168.254" }

let security_groups =
        Infra.SecurityGroups
      # [ { name = "zuul-console", rules = [ Infra.Rule::{ port = +19885 } ] }
        , { name = "monitoring"
          , rules =
            [ Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              , state = Some "absent"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              , state = Some "absent"
              }
            , Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "managesf"
          , rules =
            [ Infra.Rule::{ port = +1883 }
            , Infra.Rule::{ port = +1884 }
            , Infra.Rule::{ port = +29418 }
            , Infra.Rule::{ port = +64738 }
            , Infra.Rule::{ port = +64738, protocol = Some "udp" }
            ]
          }
        , { name = "hypervisor-oci"
          , rules =
            [ Infra.Rule::{ port = +19885 }
            , Infra.Rule::{ port = +22022, port_range_max = Some +65535 }
            ]
          }
        , { name = "prometheus-mail"
          , rules =
            [ Infra.Rule::{
              , port = +25
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        ]

let images =
      [ Infra.Image::{
        , name = "fedora-31-1.9"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/31/Cloud/x86_64/images/Fedora-Cloud-Base-31-1.9.x86_64.qcow2"
        , checksum =
            "e3c1b309d9203604922d6e255c2c5d098a309c2d46215d8fc026954f3c5c27a0"
        }
      , Infra.Image::{
        , name = "fedora-30-1.2"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/30/Cloud/x86_64/images/Fedora-Cloud-Base-30-1.2.x86_64.qcow2"
        , checksum =
            "72b6ae7b4ed09a4dccd6e966e1b3ac69bd97da419de9760b410e837ba00b4e26"
        }
      , Infra.Image::{
        , name = "centos-7-1907"
        , url =
            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
        , checksum =
            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
        }
      ]

let mkExecutors = Infra.mkServers "ze" Infra.Flavors.`4vcpus_8gb`

let mkMergers = Infra.mkServers "zm" Infra.Flavors.`1vcpu_1gb`

let volumes =
      [ { display_name = "elk-data"
        , size = 160
        , server = "elk01" ++ "." ++ fqdn
        , device = "/dev/vdb"
        }
      , { display_name = "nodepool-builder-data"
        , size = 1000
        , server = "nodepool-builder" ++ "." ++ fqdn
        , device = "/dev/vdb"
        }
      ]

let tenant-server =
      Infra.Server::(     { name = "tenant"
                          , volume_size = Some 40
                          , security_groups = [ "web" ]
                          }
                      //  Infra.OS.CentOS.`7.0`
                    )

let servers =
        [ Infra.Server::(     { name = "bridge"
                              , skip_os_server_task = True
                              , host_vars =
                                  toMap
                                    { ansible_connection =
                                        Infra.StrOrInt.str "local"
                                    }
                              }
                          //  Infra.OS.Fedora.`30`
                        )
        , Infra.Server::(     { name = "logreduce-mqtt-01"
                              , boot_from_volume = "yes"
                              , volume_size = Some 80
                              , groups = Some [ Infra.Group.logreduce-mqtt ]
                              }
                          //  Infra.OS.Fedora.`30`
                        )
        , Infra.Server::(     { name = "prometheus.monitoring"
                              , auto_ip = Some True
                              , boot_from_volume = "yes"
                              , volume_size = Some 80
                              , security_groups = [ "web" ]
                              , host_vars =
                                  toMap
                                    { podman_gw_ip =
                                        Infra.StrOrInt.str "10.88.0.1"
                                    }
                              }
                          //  Infra.OS.CentOS.`7.0`
                        )
        , Infra.Server::(     { name = "ara"
                              , auto_ip = Some True
                              , boot_from_volume = "yes"
                              , volume_size = Some 80
                              , security_groups = [ "web" ]
                              , groups = Some [ Infra.Group.ara ]
                              }
                          //  Infra.OS.Fedora.`31`
                        )
        , Infra.Server::(     { name = "redhat-oss-git-stats"
                              , auto_ip = Some True
                              , boot_from_volume = "yes"
                              , volume_size = Some 500
                              , flavor = Infra.Flavors.`8vcpus_32gb`
                              , security_groups = [ "web" ]
                              }
                          //  Infra.OS.CentOS.`7.0`
                        )
        , tenant-server // { name = "fedora" } // Infra.setIp "38.102.83.40"
        , tenant-server // { name = "ovirt" } // Infra.setIp "38.102.83.159"
        ,     tenant-server
          //  { name = "ovirt-staging" }
          //  Infra.setIp "38.102.83.251"
        , Infra.Server::(     { name = "elk01"
                              , groups = Some [ Infra.Group.sf ]
                              }
                          //  Infra.OS.CentOS.`7.0`
                        )
        , Infra.Server::(     { name = "managesf"
                              , flavor = Infra.Flavors.`4vcpus_16gb`
                              , boot_from_volume = "yes"
                              , volume_size = Some 20
                              , security_groups = [ "web", "managesf" ]
                              , groups = Some
                                  [ Infra.Group.sf
                                  , Infra.Group.install-server-sf
                                  ]
                              }
                          //  Infra.OS.CentOS.`7.0`
                          //  Infra.setIp "38.102.83.76"
                        )
        , Infra.Server::({ name = "nodepool-builder" } // Infra.OS.CentOS.`7.0`)
        , Infra.Server::(     { name = "oci01"
                              , network = "oci-private-network"
                              , security_groups = [ "hypervisor-oci" ]
                              }
                          //  Infra.OS.CentOS.`7.0`
                        )
        , Infra.Server::({ name = "zs" } // Infra.OS.CentOS.`7.0`)
        , Infra.Server::(     { name = "koji-vexxhost"
                              , image = "centos-7-1907"
                              , boot_from_volume = "yes"
                              , volume_size = Some 80
                              , flavor = Infra.Flavors.`4vcpus_8gb`
                              , security_groups = [ "web" ]
                              }
                          //  Infra.OS.CentOS.`7.0`
                          //  Infra.setIp "38.102.83.102"
                        )
        ]
      # mkExecutors 1
      # mkMergers 1

let backward-compat-name = { name = "default-router" }

in  Infra.Tenant::{
    , servers =
        Infra.setSecurityGroups
          default-security-groups
          (Infra.setFqdn fqdn servers)
    , networks =
      [ Infra.mkNetwork sf_network.name, Infra.mkNetwork oci_network.name ]
    , subnets =
      [ Infra.mkSubnet sf_network.name sf_network.network_prefix
      , Infra.mkSubnet oci_network.name oci_network.network_prefix
      ]
    , routers =
      [     Infra.mkRouter sf_network.name sf_network.network_prefix
        //  backward-compat-name
      , Infra.mkRouter oci_network.name oci_network.network_prefix
      ]
    , keypairs =
      [ { name = "sf-infra-key", public_key = Infra.sfInfraKeypair } ]
    , images = images
    , volumes = volumes
    , security_groups = security_groups
    }
