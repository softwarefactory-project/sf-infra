let Package = ../../package.dhall

let Rule = Package.Rule

let Server = Package.Server

let DefaultSecurityGroups = Package.DefaultSecurityGroups

let Image = Package.Image

let Flavors = Package.Flavors

let external-network = "public"

let cidr = "192.168.242.0/24"

let gateway_ip = "192.168.242.1"

let security_groups =
      [ { name = "common"
        , rules =
          [ Rule::{ port = +22 }, Rule::{ port = -1, protocol = Some "icmp" } ]
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
      , { name = "monitoring"
        , rules = [ Rule::{ port = +9100 }, Rule::{ port = +9101 } ]
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
      , { name = "web"
        , rules = [ Rule::{ port = +80 }, Rule::{ port = +443 } ]
        }
      ]

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
      ]

in  { servers = Package.setFqdn "softwarefactory-project.io" servers
    , networks =
      [ { name = "private-network"
        , external_network = external-network
        , port_security_enabled = False
        }
      ]
    , subnets = [ Package.Subnet::{ cidr = cidr, gateway_ip = gateway_ip } ]
    , routers =
      [ { name = "default-router"
        , network = external-network
        , interfaces = [ Package.RouterInterface::{ portip = gateway_ip } ]
        }
      ]
    , keypairs =
      [ { name = "sf-infra-key", public_key = Package.sfInfraKeypair } ]
    , images = images
    , image_cache_dir = "{{ ansible_user_dir }}/image_cache"
    , volumes = [] : List Text
    , security_groups = security_groups
    }
