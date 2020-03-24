let Infra = ../../conf/package.dhall

let fqdn = "rdoproject.org"

let Instance = Infra.Instance

let OS = (../common.dhall).OS

let Flavors = (../common.dhall).Flavors

let instances =
      [ Instance::{
        , name = "mirror.regionone.vexxhost"
        , groups = [ Infra.Group.afs-mirror ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , security_groups = [ "web", "afs" ]
          , volume_size = Some 200
          }
        }
      , Instance::{
        , name = "centos8-rpm-packaging-ci"
        , groups = [ Infra.Group.dlrn ]
        , connection = OS.CentOS.`8.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`8.0`.image.name
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 100
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "centos8-rpm-packaging-swap"
            , size = 8
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "rpm-packaging-ci"
        , groups = [ Infra.Group.dlrn ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , flavor = Some "ci.m1.large"
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 100
          }
        }
      , Instance::{
        , name = "registry-vexxhost"
        , groups =
          [ Infra.Group.registry
          , Infra.Group.etcd
          , Infra.Group.masters
          , Infra.Group.nodes
          , Infra.Group.openshift
          , Infra.Group.installer
          ]
        , connection =
            OS.CentOS.`7.0`.connection // { ansible_become = Some True }
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , auto_ip = Some True
          , security_groups = [ "web", "registry" ]
          , volume_size = Some 200
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "registry-docker"
            , size = 100
            , device = "/dev/vdb"
            }
          , Infra.Volume::{
            , display_name = "registry-data"
            , size = 1024
            , device = "/dev/vdc"
            }
          ]
        }
      , Instance::{
        , name = "trunk-centos8"
        , groups = [ Infra.Group.dlrn ]
        , connection = OS.CentOS.`8.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`8.0`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 512
          }
        }
      , Instance::{
        , name = "trunk-centos7"
        , groups = [ Infra.Group.dlrn ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 512
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "trunk-centos7-swap"
            , size = 8
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "review"
        , groups = [ Infra.Group.rdo, Infra.Group.install-server ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , flavor = Some Flavors.`1vcpu_4gb`
          , auto_ip = Some True
          , volume_size = Some 40
          , security_groups = [ "web", "managesf", "internal" ]
          }
        }
      , Instance::{
        , name = "elk"
        , groups = [ Infra.Group.rdo ]
        , connection = OS.CentOS.`7.0`.connection
        , server =
                Infra.Server::{
                , image = OS.CentOS.`7.0`.image.name
                , security_groups = [ "elk", "internal" ]
                }
            //  Infra.setIp "38.102.83.136"
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
        , name = "logserver"
        , groups = [ Infra.Group.rdo ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , security_groups = [ "web" ]
          , volume_size = Some 10
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "logs-data"
            , size = 1000
            , device = "/dev/vdb"
            }
          , Infra.Volume::{
            , display_name = "logs-data02"
            , size = 1000
            , device = "/dev/vdc"
            }
          , Infra.Volume::{
            , display_name = "logs-data03"
            , size = 1000
            , device = "/dev/vdd"
            }
          ]
        }
      , Instance::{
        , name = "images"
        , connection = OS.CentOS.`8.1`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`8.1`.image.name
          , flavor = Some Flavors.`1vcpu_4gb`
          , auto_ip = Some True
          , security_groups = [ "web", "rcn-share" ]
          , volume_size = Some 50
          }
        , volumes =
          [ { display_name = "images-data"
            , size = 1000
            , server = "images.rdoproject.org"
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "www"
        , connection = OS.CentOS.`8.1`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`8.1`.image.name
          , flavor = Some Flavors.`1vcpu_4gb`
          , auto_ip = Some True
          , security_groups = [ "web" ]
          , volume_size = Some 10
          }
        }
      , Instance::{
        , name = "dlrn-db"
        , connection = OS.CentOS.`8.1`.connection
        , server = Infra.Server::{
          , image = OS.CentOS.`8.1`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , auto_ip = Some True
          , security_groups = [ "dlrn-db" ]
          , volume_size = Some 60
          }
        }
      ]

let AwsServer =
          (../common.dhall).ExternalServer
      //  { connection = OS.CentOS.`7.0`.connection // { ansible_port = 3300 } }

let extra =
      [ Instance::(     { name = "backup", groups = [ Infra.Group.backup ] }
                    //  AwsServer
                  )
      , Instance::(     { name = "trunk", groups = [ Infra.Group.dlrn ] }
                    //  AwsServer
                  )
      ]

in  Infra.setSecurityGroups
      [ "common", "monitoring" ]
      (Infra.setFqdn fqdn (instances # extra))
