let Infra = ../../Infra/package.dhall

let fqdn = "softwarefactory-project.io"

let Instance = Infra.Instance

let OS = (../common.dhall).OS

let Flavors = (../common.dhall).Flavors

let tenant-instance =
      Instance::{
      , name = "tenant"
      , groups = [ "rdocloud-data-fetcher", "sf" ]
      , connection = OS.CentOS.`7.0`.connection
      }

let tenant-server =
      Infra.Server::{
      , image = OS.CentOS.`7.0`.image.name
      , volume_size = Some 40
      , security_groups = [ "web" ]
      }

let tenant-instances =
      [     tenant-instance
        //  { name = "fedora"
            , server = Some (tenant-server // Infra.Server.Ip "38.102.83.40")
            }
      ,     tenant-instance
        //  { name = "ovirt"
            , server = Some (tenant-server // Infra.Server.Ip "38.102.83.159")
            }
      ,     tenant-instance
        //  { name = "ovirt-staging"
            , server = Some (tenant-server // Infra.Server.Ip "38.102.83.251")
            }
      ,     tenant-instance
        //  { name = "ansible"
            , server = Some
                (     tenant-server
                  //  Infra.Server.Ip "38.102.83.19"
                  //  { state =
                          let note = "force server creation" in Some "present"
                      }
                )
            }
      ]

let instances =
      [ Instance::{ name = "bridge", connection = OS.Fedora.`31`.connection }
      , Instance::{
        , name = "logreduce-mqtt-01"
        , groups = [ "logreduce-mqtt" ]
        , connection = OS.Fedora.`30`.connection
        , server = Some Infra.Server::{
          , image = OS.Fedora.`30`.image.name
          , boot_from_volume = "yes"
          , volume_size = Some 80
          }
        }
      , Instance::{
        , name = "prometheus.monitoring"
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = [ "web", "pushprox-proxy", "prometheus" ]
          }
        }
      , Instance::{
        , name = "ara"
        , auth_urls = [ "https://ara.softwarefactory-project.io" ]
        , groups = [ "ara" ]
        , connection = OS.Fedora.`31`.connection
        , server = Some Infra.Server::{
          , image = OS.Fedora.`31`.image.name
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = [ "web" ]
          }
        }
      , Instance::{
        , name = "redhat-oss-git-stats"
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 500
          , flavor = Some Flavors.`8vcpus_32gb`
          , security_groups = [ "web" ]
          }
        }
      , Instance::{
        , name = "elk"
        , groups = [ "sf", "rdocloud-data-fetcher" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , security_groups = [ "elk" ]
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "elk-data"
            , size = 160
            , server = "elk" ++ "." ++ fqdn
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "managesf"
        , urls =
            let note = "TODO: move urls to relevant instance"

            let validate =
                  "curl -o /dev/null -s -w '%{http_code}' URL should return 200"

            in  [ "https://softwarefactory-project.io"
                , "https://softwarefactory-project.io/analytics/api/licensing/info"
                , "https://softwarefactory-project.io/zuul/api/info"
                , "https://softwarefactory-project.io/elasticsearch/"
                , "https://ovirt.softwarefactory-project.io/zuul/api/info"
                , "https://ansible.softwarefactory-project.io/zuul/api/info"
                , "https://fedora.softwarefactory-project.io/zuul/api/info"
                , "https://www.softwarefactory-project.io"
                , "https://images.rdoproject.org"
                , "https://lists.rdoproject.org"
                , "https://logserver.rdoproject.org"
                , "http://mirror.regionone.rdo-cloud.rdoproject.org"
                , "http://mirror.regionone.vexxhost.rdoproject.org"
                , "https://trunk.rdoproject.org"
                , "https://trunk.registry.rdoproject.org"
                , "https://softwarefactory-project.io/elasticsearch/_cluster/health?wait_for_status=green&timeout=50s"
                , "https://rdo.vexxhost.ca/auth/login/?next=/"
                ]
        , groups = [ "sf", "install-server", "rdocloud-data-fetcher" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.CentOS.`7.0`.image.name
                  , flavor = Some Flavors.`4vcpus_16gb`
                  , boot_from_volume = "yes"
                  , volume_size = Some 20
                  , security_groups = [ "web", "managesf", "zookeeper" ]
                  }
              //  Infra.Server.Ip "38.102.83.76"
            )
        }
      , Instance::{
        , name = "nodepool-builder"
        , groups = [ "sf", "rdocloud-data-fetcher" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{ image = OS.CentOS.`7.0`.image.name }
        , volumes =
          [ Infra.Volume::{
            , display_name = "nodepool-builder-data-2"
            , size = 200
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "k1s02"
        , groups = [ "sf" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.CentOS.`7.0`.image.name
                  , network = "oci-private-network"
                  , security_groups = [ "hypervisor-oci" ]
                  , flavor = Some Flavors.`8vcpu_16GB`
                  }
              //  Infra.Server.Ip "38.102.83.186"
            )
        }
      , Instance::{
        , name = "zs"
        , groups = [ "sf" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{ image = OS.CentOS.`7.0`.image.name }
        }
      , Instance::{
        , name = "koji"
        , connection = OS.CentOS.`7.0`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.CentOS.`7.0`.image.name
                  , boot_from_volume = "yes"
                  , volume_size = Some 80
                  , flavor = Some Flavors.`4vcpus_8gb`
                  , security_groups = [ "web" ]
                  }
              //  Infra.Server.Ip "38.102.83.102"
            )
        }
      , Instance::{
        , name = "integrations"
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 20
          , flavor = Some Flavors.`1vcpu_2gb`
          }
        }
      , Instance::{
        , name = "nodepool-launcher-02"
        , connection = Infra.Connection::{
          , ansible_user = "centos"
          , ansible_host = Some "38.145.38.40"
          }
        }
      ]

let mkServers =
      \(name : Text) ->
      \(flavor : Text) ->
        Infra.Instance.generate
          ( \(idx : Natural) ->
              Instance::{
              , name = "${name}0${Natural/show idx}"
              , groups = [ "sf" ]
              , connection = OS.CentOS.`7.0`.connection
              , server = Some Infra.Server::{
                , image = OS.CentOS.`7.0`.image.name
                , flavor = Some flavor
                , boot_from_volume = "no"
                }
              }
          )

let mkExecutors = mkServers "ze" Flavors.`4vcpus_8gb`

let mkMergers = mkServers "zm" Flavors.`1vcpu_1gb`

let zuuls = mkExecutors 7 # mkMergers 8

let default-security-groups = [ "common", "monitoring", "internal" ]

in  Infra.Tenant.addSecurityGroupsAndSetFqdn
      default-security-groups
      fqdn
      (instances # tenant-instances # zuuls)
