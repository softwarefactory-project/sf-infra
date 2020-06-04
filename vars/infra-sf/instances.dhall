let Infra = ../../conf/package.dhall

let fqdn = "softwarefactory-project.io"

let Instance = Infra.Instance

let OS = Infra.OS

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
      ,     tenant-instance
        //  { name = "ansible"
            , server =
                    tenant-instance.server
                //  Infra.setIp "38.102.83.19"
                //  { state =
                        let note = "force server creation" in Some "present"
                    }
            }
      ]

let instances =
      [ Instance::(     { name = "bridge"
                        , connection = OS.Fedora.`31`.connection
                        }
                    //  Infra.ExternalServer
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
          , security_groups = [ "web", "pushprox-proxy", "prometheus" ]
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
        , server = Infra.Server::{
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
        , groups =
          [ Infra.Group.sf
          , Infra.Group.install-server
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
            , display_name = "nodepool-builder-data-2"
            , size = 200
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "k1s02"
        , groups = [ Infra.Group.sf ]
        , connection = OS.CentOS.`7.0`.connection
        , server =
                Infra.Server::{
                , image = OS.CentOS.`7.0`.image.name
                , network = "oci-private-network"
                , security_groups = [ "hypervisor-oci" ]
                , flavor = Some Flavors.`8vcpu_16GB`
                }
            //  Infra.setIp "38.102.83.186"
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
      , Instance::{
        , name = "integrations"
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 20
          , flavor = Some Flavors.`1vcpu_2gb`
          }
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

let default-security-groups = [ "common", "monitoring", "internal" ]

in  Infra.setSecurityGroups
      default-security-groups
      (Infra.setFqdn fqdn (instances # tenant-instances # zuuls))
