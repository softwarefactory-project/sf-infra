let Package = ../../package.dhall

let Rule = Package.Rule

let Image = Package.Image

let mkServers = Package.mkServers

let mkNetwork = Package.mkNetwork

let mkSubnet = Package.mkSubnet

let mkRouter = Package.mkRouter

let Flavors = Package.Flavors

let Server = Package.Server

let fqdn = "softwarefactory-project.io"

let sf_network = { name = "private", network_prefix = "192.168.242" }

let oci_network = { name = "oci-private", network_prefix = "192.168.254" }

let security_groups =
        Package.SecurityGroups
      # [ { name = "monitoring"
          , rules = [ Rule::{ port = +9100 }, Rule::{ port = +9101 } ]
          }
        , { name = "zuul-console", rules = [ Rule::{ port = +19885 } ] }
        , { name = "private-monitoring"
          , rules =
            [ Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              }
            , Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
              }
            , Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              }
            , Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
              }
            ]
          }
        , { name = "managesf"
          , rules =
            [ Rule::{ port = +1883 }
            , Rule::{ port = +1884 }
            , Rule::{ port = +29418 }
            , Rule::{ port = +64738 }
            , Rule::{ port = +64738, protocol = Some "udp" }
            ]
          }
        , { name = "hypervisor-oci"
          , rules =
            [ Rule::{ port = +19885 }
            , Rule::{ port = +22022, port_range_max = Some +65535 }
            ]
          }
        , { name = "public-monitoring"
          , rules = [ Rule::{ port = +9090 }, Rule::{ port = +3000 } ]
          }
        , { name = "prometheus-mail"
          , rules =
            [ Rule::{
              , port = +25
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        ]

let DefaultSecurityGroups = [ "common", "private-monitoring" ]

let images =
      [ Image::{
        , name = "fedora-31-1.9"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/31/Cloud/x86_64/images/Fedora-Cloud-Base-31-1.9.x86_64.qcow2"
        , checksum =
            "e3c1b309d9203604922d6e255c2c5d098a309c2d46215d8fc026954f3c5c27a0"
        }
      , Image::{
        , name = "fedora-30-1.2"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/30/Cloud/x86_64/images/Fedora-Cloud-Base-30-1.2.x86_64.qcow2"
        , checksum =
            "72b6ae7b4ed09a4dccd6e966e1b3ac69bd97da419de9760b410e837ba00b4e26"
        }
      , Image::{
        , name = "centos-7-1907"
        , url =
            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
        , checksum =
            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
        }
      ]

let mkExecutors = mkServers "ze" Flavors.`1cpu_4gig`

let mkMergers = mkServers "zm" Flavors.`2cpus_8gig`

let volumes =
      [ { display_name = "elk-data"
        , size = 160
        , server = "elk" ++ "." ++ fqdn
        , device = "/dev/vdb"
        }
      , { display_name = "nodepool-builder-data"
        , size = 10
        , server = "nodepool-builder" ++ "." ++ fqdn
        , device = "/dev/vdb"
        }
      ]

let servers =
        [ Server::{
          , name = "logreduce-mqtt-01"
          , image = "fedora-30-1.2"
          , boot_from_volume = "yes"
          , volume_size = Some 80
          }
        , Server::{
          , name = "prometheus.monitoring"
          , floating_ip = "yes"
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = [ "common", "public-monitoring", "web" ]
          }
        , Server::{
          , name = "ara"
          , floating_ip = "yes"
          , image = "fedora-31-1.9"
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = DefaultSecurityGroups # [ "web" ]
          }
        , Server::{
          , name = "redhat-oss-git-stats"
          , floating_ip = "yes"
          , image = "fedora-31-1.9"
          , boot_from_volume = "yes"
          , volume_size = Some 500
          , flavor = Flavors.`8cpus_32gig`
          , security_groups = DefaultSecurityGroups # [ "web" ]
          }
        , Server::{ name = "elk" }
        , Server::{
          , name = "managesf"
          , flavor = Flavors.`4cpus_16gig`
          , boot_from_volume = "yes"
          , volume_size = Some 20
          , security_groups = [ "common", "web", "managesf" ]
          , volumes = Some [ "elk-data" ]
          }
        , Server::{ name = "nodepool-builder" }
        , Server::{
          , name = "oci01"
          , network = "oci-private-network"
          , security_groups = [ "common", "hypervisor-oci" ]
          , volumes = Some [ "nodepool-builder-data" ]
          }
        , Server::{ name = "zs" }
        ]
      # mkExecutors 1
      # mkMergers 1

let backward-compat-name = { name = "default-router" }

in  { servers = Package.setFqdn fqdn servers
    , networks = [ mkNetwork sf_network.name, mkNetwork oci_network.name ]
    , subnets =
      [ mkSubnet sf_network.name sf_network.network_prefix
      , mkSubnet oci_network.name oci_network.network_prefix
      ]
    , routers =
      [     mkRouter sf_network.name sf_network.network_prefix
        //  backward-compat-name
      , mkRouter oci_network.name oci_network.network_prefix
      ]
    , keypairs =
      [ { name = "sf-infra-key", public_key = Package.sfInfraKeypair } ]
    , images = images
    , image_cache_dir = "{{ ansible_user_dir }}/image_cache"
    , volumes = volumes
    , security_groups = security_groups
    }
