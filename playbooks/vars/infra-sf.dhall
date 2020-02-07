let Infra = ../../conf/infra.dhall

let fqdn = "softwarefactory-project.io"

let sf_network = { name = "private", network_prefix = "192.168.242" }

let oci_network = { name = "oci-private", network_prefix = "192.168.254" }

let security_groups =
        Infra.SecurityGroups
      # [ { name = "monitoring"
          , rules =
            [ Infra.Rule::{ port = +9100 }, Infra.Rule::{ port = +9101 } ]
          }
        , { name = "zuul-console", rules = [ Infra.Rule::{ port = +19885 } ] }
        , { name = "private-monitoring"
          , rules =
            [ Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
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
        , { name = "public-monitoring"
          , rules =
            [ Infra.Rule::{ port = +9090 }, Infra.Rule::{ port = +3000 } ]
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

let DefaultSecurityGroups = [ "common", "private-monitoring" ]

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
        , server = "elk" ++ "." ++ fqdn
        , device = "/dev/vdb"
        }
      , { display_name = "nodepool-builder-data"
        , size = 1000
        , server = "nodepool-builder" ++ "." ++ fqdn
        , device = "/dev/vdb"
        }
      ]

let ips = { managesf = Infra.setIp "38.102.83.76" }

let servers =
        [ Infra.Server::{
          , name = "logreduce-mqtt-01"
          , image = "fedora-30-1.2"
          , boot_from_volume = "yes"
          , volume_size = Some 80
          }
        , Infra.Server::{
          , name = "prometheus.monitoring"
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = [ "common", "public-monitoring", "web" ]
          }
        , Infra.Server::{
          , name = "ara"
          , auto_ip = Some True
          , image = "fedora-31-1.9"
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = DefaultSecurityGroups # [ "web" ]
          }
        , Infra.Server::{
          , name = "redhat-oss-git-stats"
          , auto_ip = Some True
          , image = "fedora-31-1.9"
          , boot_from_volume = "yes"
          , volume_size = Some 500
          , flavor = Infra.Flavors.`8vcpus_32gb`
          , security_groups = DefaultSecurityGroups # [ "web" ]
          }
        , Infra.Server::{ name = "elk" }
        , ips.managesf
            Infra.Server::{
            , name = "managesf"
            , flavor = Infra.Flavors.`4vcpus_16gb`
            , boot_from_volume = "yes"
            , volume_size = Some 20
            , security_groups = [ "common", "web", "managesf" ]
            , volumes = Some [ "elk-data" ]
            }
        , Infra.Server::{ name = "nodepool-builder" }
        , Infra.Server::{
          , name = "oci01"
          , network = "oci-private-network"
          , security_groups = [ "common", "hypervisor-oci" ]
          , volumes = Some [ "nodepool-builder-data" ]
          }
        , Infra.Server::{ name = "zs" }
        ]
      # mkExecutors 1
      # mkMergers 1

let backward-compat-name = { name = "default-router" }

in  { servers = Infra.setFqdn fqdn servers
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
    , image_cache_dir = "{{ ansible_user_dir }}/image_cache"
    , volumes = volumes
    , security_groups = security_groups
    }
