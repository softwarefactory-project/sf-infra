let Infra = ../../conf/package.dhall

let fqdn = "softwarefactory-project.io"

let Instance = Infra.Instance

let OS = (../common.dhall).OS

let Flavors = (../common.dhall).Flavors

let tenant-instance =
      Instance::{
      , name = "tenant"
      , groups = [ Infra.Group.rdocloud-data-fetcher, Infra.Group.sf ]
      , connection = OS.CentOS.`7.0`.connection
      , server = Infra.Server::{
        , image = OS.CentOS.`7.0`.image.name
        , volume_size = Some 40
        , security_groups = [ "web" ]
        }
      }

let tenant-instances =
      [     tenant-instance
        //  { name = "fedora"
            , server = tenant-instance.server // Infra.setIp "38.102.83.40"
            }
      ,     tenant-instance
        //  { name = "ovirt"
            , server = tenant-instance.server // Infra.setIp "38.102.83.159"
            }
      ,     tenant-instance
        //  { name = "ovirt-staging"
            , server = tenant-instance.server // Infra.setIp "38.102.83.251"
            }
      ]

let instances =
      [ Instance::(     { name = "bridge"
                        , connection = OS.Fedora.`31`.connection
                        }
                    //  (../common.dhall).ExternalServer
                  )
      , Instance::{
        , name = "logreduce-mqtt-01"
        , groups = [ Infra.Group.logreduce-mqtt ]
        , connection = OS.Fedora.`30`.connection
        , server = Infra.Server::{
          , image = OS.Fedora.`30`.image.name
          , boot_from_volume = "yes"
          , volume_size = Some 80
          }
        }
      , Instance::{
        , name = "prometheus.monitoring"
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = [ "web" ]
          }
        }
      , Instance::{
        , name = "ara"
        , groups = [ Infra.Group.ara ]
        , connection = OS.Fedora.`31`.connection
        , server = Infra.Server::{
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
        , server = Infra.Server::{
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
        , groups = [ Infra.Group.sf, Infra.Group.rdocloud-data-fetcher ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{ image = OS.CentOS.`7.0`.image.name }
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
        , groups =
          [ Infra.Group.sf
          , Infra.Group.install-server-sf
          , Infra.Group.rdocloud-data-fetcher
          ]
        , connection = OS.CentOS.`7.0`.connection
        , server =
                Infra.Server::{
                , image = OS.CentOS.`7.0`.image.name
                , flavor = Some Flavors.`4vcpus_16gb`
                , boot_from_volume = "yes"
                , volume_size = Some 20
                , security_groups = [ "web", "managesf" ]
                }
            //  Infra.setIp "38.102.83.76"
        }
      , Instance::{
        , name = "nodepool-builder"
        , groups = [ Infra.Group.sf, Infra.Group.rdocloud-data-fetcher ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{ image = OS.CentOS.`7.0`.image.name }
        , volumes =
          [ Infra.Volume::{
            , display_name = "nodepool-builder-data"
            , size = 1000
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "oci01"
        , groups = [ Infra.Group.sf ]
        , connection = OS.CentOS.`7.0`.connection
        , server =
                Infra.Server::{
                , image = OS.CentOS.`7.0`.image.name
                , network = "oci-private-network"
                , security_groups = [ "hypervisor-oci" ]
                }
            //  Infra.setIp "38.102.83.245"
        }
      , Instance::{
        , name = "zs"
        , groups = [ Infra.Group.sf ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{ image = OS.CentOS.`7.0`.image.name }
        }
      , Instance::{
        , name = "koji"
        , connection = OS.CentOS.`7.0`.connection
        , server =
                Infra.Server::{
                , image = OS.CentOS.`7.0`.image.name
                , boot_from_volume = "yes"
                , volume_size = Some 80
                , flavor = Some Flavors.`4vcpus_8gb`
                , security_groups = [ "web" ]
                }
            //  Infra.setIp "38.102.83.102"
        }
      ]

let mkServers =
          \(name : Text)
      ->  \(flavor : Text)
      ->  \(count : Natural)
      ->  Infra.Prelude.List.map
            Natural
            Instance.Type
            (     \(idx : Natural)
              ->  Instance::{
                  , name = "${name}0${Natural/show idx}"
                  , groups = [ Infra.Group.sf ]
                  , connection = OS.CentOS.`7.0`.connection
                  , server = Infra.Server::{
                    , image = OS.CentOS.`7.0`.image.name
                    , flavor = Some flavor
                    , boot_from_volume = "no"
                    }
                  }
            )
            (Infra.seq count)

let mkExecutors = mkServers "ze" Flavors.`4vcpus_8gb`

let mkMergers = mkServers "zm" Flavors.`1vcpu_1gb`

let zuuls = mkExecutors 7 # mkMergers 8

let default-security-groups = [ "common", "monitoring" ]

in  Infra.setSecurityGroups
      default-security-groups
      (Infra.setFqdn fqdn (instances # tenant-instances # zuuls))
