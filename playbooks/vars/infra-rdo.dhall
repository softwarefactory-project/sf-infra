let Infra = ../../conf/infra.dhall

let fqdn = "rdoproject.org"

let rdo_network = { name = "private", network_prefix = "192.168.240" }

let backward-compat-name = { name = "default-router" }

let security_groups =
        Infra.SecurityGroups
      # [ { name = "afs"
          , rules =
            [ Infra.Rule::{ port = +8080 }
            , Infra.Rule::{ port = +8081 }
            , Infra.Rule::{ port = +8082 }
            ]
          }
        , { name = "monitoring"
          , rules =
            [ Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ bridge_public_ip }}/32"
              , state = Some "absent"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "rdo-trunk", rules = [ Infra.Rule::{ port = +3300 } ] }
        , { name = "registry"
          , rules =
            [ Infra.Rule::{ port = +53 }
            , Infra.Rule::{ port = +2379 }
            , Infra.Rule::{ port = +2380 }
            , Infra.Rule::{ port = +4001 }
            , Infra.Rule::{ port = +5000 }
            , Infra.Rule::{ port = +8443 }
            , Infra.Rule::{ port = +9090 }
            , Infra.Rule::{ port = +10250 }
            , Infra.Rule::{ port = +4789, protocol = Some "udp" }
            , Infra.Rule::{ port = +8053, protocol = Some "udp" }
            ]
          }
        , { name = "rcn-share"
          , rules =
            [ Infra.Rule::{
              , port = +4433
              , remote_ip_prefix = Some "38.145.32.0/22"
              , protocol = Some "tcp"
              }
            ]
          }
        ]

let images =
      [ Infra.Image::{
        , name = "centos-7-1907"
        , url =
            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
        , checksum =
            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
        }
      , Infra.Image::{
        , name = "fedora-30-1.2"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/30/Cloud/x86_64/images/Fedora-Cloud-Base-30-1.2.x86_64.qcow2"
        , checksum =
            "72b6ae7b4ed09a4dccd6e966e1b3ac69bd97da419de9760b410e837ba00b4e26"
        }
      , Infra.Image::{
        , name = "centos-8.0-1905"
        , url =
            "https://jpena.fedorapeople.org/CentOS-8-GenericCloud-8.0.1905-22.x86_64.qcow2"
        , checksum =
            "c86c119665866695a700a4ab523c774c64ed7e0dd9e6e89f5f032e0f03148a47"
        }
      , Infra.Image::{
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
      , { display_name = "logs-data02"
        , size = 1000
        , server = "logserver.rdoproject.org"
        , device = "/dev/vdc"
        }
      , { display_name = "images-data"
        , size = 1000
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
      Infra.setSecurityGroups
        [ "common", "monitoring" ]
        [ Infra.Server::{
          , name = "mirror.regionone.vexxhost"
          , auto_ip = Some True
          , security_groups = [ "web", "afs" ]
          , volume_size = Some 200
          }
        , Infra.Server::{
          , name = "centos8-rpm-packaging-ci"
          , image = "centos-8.0-1905"
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 100
          }
        , Infra.Server::{
          , name = "rpm-packaging-ci"
          , flavor = "ci.m1.large"
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 100
          }
        , Infra.Server::{
          , name = "fedora-rpm-packaging-ci"
          , image = "fedora-30-1.2"
          , flavor = "ci.m1.large"
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 100
          }
        , Infra.Server::{
          , name = "registry-vexxhost"
          , flavor = Infra.Flavors.`4vcpus_16gb`
          , auto_ip = Some True
          , security_groups = [ "web", "registry" ]
          , volume_size = Some 200
          }
        , Infra.Server::{
          , name = "trunk-centos8"
          , image = "centos-8.0-1905"
          , flavor = Infra.Flavors.`4vcpus_16gb`
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 512
          }
        , Infra.Server::{
          , name = "trunk-centos7"
          , flavor = Infra.Flavors.`4vcpus_16gb`
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 512
          }
        , Infra.Server::{
          , name = "install-server"
          , flavor = Infra.Flavors.`1vcpu_4gb`
          , auto_ip = Some True
          , volume_size = Some 40
          }
        , Infra.Server::{
          , name = "logserver"
          , auto_ip = Some True
          , security_groups = [ "web" ]
          , volume_size = Some 10
          }
        , Infra.Server::{
          , name = "images-vexxhost"
          , image = "centos-8.1-1911"
          , flavor = Infra.Flavors.`1vcpu_4gb`
          , auto_ip = Some True
          , security_groups = [ "web", "rcn-share" ]
          , volume_size = Some 50
          }
        , Infra.Server::{
          , name = "www"
          , image = "centos-8.1-1911"
          , flavor = Infra.Flavors.`1vcpu_4gb`
          , auto_ip = Some True
          , security_groups = [ "web" ]
          , volume_size = Some 10
          }
        ]

in  Infra.Tenant::{
    , servers = Infra.setFqdn fqdn servers
    , networks = [ Infra.mkNetwork rdo_network.name ]
    , subnets = [ Infra.mkSubnet rdo_network.name rdo_network.network_prefix ]
    , routers =
      [     Infra.mkRouter rdo_network.name rdo_network.network_prefix
        //  backward-compat-name
      ]
    , keypairs =
      [ { name = "sf-infra-key", public_key = Infra.sfInfraKeypair } ]
    , images = images
    , volumes = volumes
    , security_groups = security_groups
    }
