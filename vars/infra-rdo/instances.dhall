let Infra = ../../Infra/package.dhall

let fqdn = "rdoproject.org"

let Instance = Infra.Instance

let OS = (../common.dhall).OS

let Flavors = (../common.dhall).Flavors

let instances =
      [ Instance::{
        , name = "mirror.regionone.vexxhost"
        , groups = [ "afs-mirror" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , security_groups = [ "web", "afs" ]
          , volume_size = Some 200
          }
        }
      , Instance::{
        , name = "centos8-rpm-packaging-ci"
        , groups = [ "dlrn" ]
        , connection = OS.CentOS.`8.0`.connection
        , server = Some Infra.Server::{
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
        , groups = [ "dlrn" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , flavor = Some "ci.m1.large"
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 100
          }
        }
      , Instance::{
        , name = "registry"
        , groups =
          [ "registry", "etcd", "masters", "nodes", "openshift", "installer" ]
        , connection =
            OS.CentOS.`7.0`.connection // { ansible_become = Some True }
        , server = Some Infra.Server::{
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
        , groups = [ "dlrn" ]
        , connection = OS.CentOS.`8.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`8.0`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , auto_ip = Some True
          , security_groups = [ "web", "rdo-trunk" ]
          , volume_size = Some 512
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "trunk-centos8-swap"
            , size = 16
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "trunk-centos7"
        , groups = [ "dlrn" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
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
        , name = "managesf.review"
        , urls =
            let note = "TODO: move urls to relevant instance"

            in  [ "https://review.rdoproject.org/zuul/api/info"
                , "https://review.rdoproject.org/analytics/app/kibana"
                , "https://review.rdoproject.org"
                , "https://review.rdoproject.org/elasticsearch/_cluster/health?wait_for_status=green&timeout=50s"
                ]
        , groups = [ "rdo", "install-server" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , auto_ip = Some True
          , volume_size = Some 40
          , security_groups = [ "web", "managesf", "internal" ]
          }
        }
      , Instance::{
        , name = "elk"
        , groups = [ "rdo" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.CentOS.`7.0`.image.name
                  , security_groups = [ "elk", "internal" ]
                  }
              //  Infra.Server.Ip "38.102.83.136"
            )
        , volumes =
          [ Infra.Volume::{
            , display_name = "elk-data"
            , size = 1000
            , server = "elk" ++ "." ++ fqdn
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "logserver"
        , groups = [ "rdo" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , security_groups = [ "web", "hound", "internal" ]
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
        , server = Some Infra.Server::{
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
        , server = Some Infra.Server::{
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
        , server = Some Infra.Server::{
          , image = OS.CentOS.`8.1`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , auto_ip = Some True
          , security_groups = [ "dlrn-db" ]
          , volume_size = Some 60
          }
        }
      ]

let mkCentosWorker =
      Instance.generate
        ( \(idx : Natural) ->
            Instance::{
            , name = "rdo-ci-cloudslave0${Natural/show idx}.ci.centos.org"
            , groups = [ "ci-centos-org" ]
            , connection = Infra.Connection::{
              , ansible_user = "rdo-monitoring"
              , proxy_command = Some
                  "ssh -q rdo-monitoring@jump.ci.centos.org -W %h:%p"
              }
            }
        )

let AwsServer =
      { connection = OS.CentOS.`7.0`.connection // { ansible_port = 3300 } }

let extra =
      [ Instance::({ name = "backup", groups = [ "backup" ] } // AwsServer)
      , Instance::({ name = "trunk", groups = [ "dlrn" ] } // AwsServer)
      , Instance::{
        , name = "mirror.regionone.rdo-cloud"
        , groups = [ "afs-mirror" ]
        , connection = Infra.Connection::{ ansible_user = "centos" }
        }
      ]

let vexxhost-instances =
      Infra.Tenant.addSecurityGroupsAndSetFqdn
        [ "common", "monitoring" ]
        fqdn
        (instances # extra)

let centos-instances = mkCentosWorker 5

in  vexxhost-instances # centos-instances
