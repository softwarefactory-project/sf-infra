let Prelude = ../../Infra/Prelude.dhall

let Infra = ../../Infra/package.dhall

let fqdn = "rdoproject.org"

let Instance = Infra.Instance

let OS = (../common.dhall).OS

let Flavors = (../common.dhall).Flavors

let instances =
      [ Instance::{
        , name = "mirror.regionone.vexxhost"
        , groups = [ "afs-mirror", "rdo" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , security_groups = [ "web", "afs", "apache_exporter" ]
          , floating_ip = Some True
          , volume_size = Some 200
          }
        }
      , Instance::{
        , name = "quay"
        , groups = [ "quay", "rdo", "rhel" ]
        , connection =
            OS.RHEL.`9.3`.connection // { ansible_host = Some "38.129.56.158" }
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , floating_ip = Some True
          , security_groups = [ "web" ]
          , volume_size = Some 50
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "quay_db"
            , size = 50
            , device = "/dev/vdb"
            }
          , Infra.Volume::{
            , display_name = "quay_data"
            , size = 1024
            , device = "/dev/vdc"
            }
          , Infra.Volume::{
            , display_name = "quay_data_2"
            , size = 512
            , device = "/dev/vdd"
            }
          ]
        , monitoring_urls = [ "https://quay.rdoproject.org/" ]
        }
      , Instance::{
        , name = "trunk-centos8"
        , groups = [ "dlrn" ]
        , connection = OS.CentOS.`8.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`8.0`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , floating_ip = Some True
          , security_groups = [ "web", "rdo-trunk", "apache_exporter" ]
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
        , name = "managesf.review"
        , backup = Some Infra.Backup::{
          , run_sf_backup = True
          , real_name = Some "review.rdoproject.org"
          , instances = Some [ "review.rdoproject.org" ]
          , dir = Some "/var/lib/backup/bup/review.rdoproject.org"
          , domain = Some "review.rdoproject.org"
          , month_subdir = Some 1
          }
        , monitoring_urls =
            let note = "TODO: move urls to relevant instance"

            in  [ "https://review.rdoproject.org/zuul/api/info"
                , "https://review.rdoproject.org"
                ]
        , groups = [ "rhel", "rdo", "install-server", "backup-rdo" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , boot_from_volume = "yes"
          , floating_ip = Some True
          , volume_size = Some 100
          , security_groups = [ "web", "managesf", "apache_exporter" ]
          }
        }
      , Instance::{
        , name = "logserver"
        , groups = [ "rhel", "rdo" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , boot_from_volume = "yes"
          , volume_size = Some 20
          , floating_ip = Some True
          , security_groups = [ "web", "hound", "internal", "apache_exporter" ]
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
        , name = "www"
        , groups = [ "osci" ]
        , connection =
                OS.CentOS.`8.1`.connection
            //  { ansible_python_interpreter = "python3" }
        , server = Some Infra.Server::{
          , image = OS.CentOS.`8.1`.image.name
          , flavor = Some Flavors.`1vcpu_4gb`
          , floating_ip = Some True
          , security_groups = [ "web", "apache_exporter" ]
          , volume_size = Some 10
          }
        , monitoring_urls = [ "https://www.rdoproject.org/" ]
        }
      , Instance::{
        , name = "dlrn-db"
        , groups = [ "dlrn" ]
        , connection = OS.CentOS.`8.1`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`8.1`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , floating_ip = Some True
          , security_groups = [ "dlrn-db" ]
          , volume_size = Some 60
          }
        }
      , Instance::{
        , name = "opensearch"
        , groups = [ "opensearch", "rdo", "rhel" ]
        , connection =
            OS.RHEL.`9.3`.connection // { ansible_host = Some "38.102.83.14" }
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`4vcpus_8gb`
          , floating_ip = Some True
          , security_groups =
            [ "web", "opensearch", "internal", "apache_exporter" ]
          , volume_size = Some 50
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "opensearch_data"
            , size = 1024
            , device = "/dev/vdb"
            }
          ]
        , monitoring_urls = [ "https://opensearch.rdoproject.org/" ]
        }
      , Instance::{
        , name = "dlrn-db-centos9"
        , groups = [ "dlrn-centos9", "rhel", "rdo", "db_master" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , floating_ip = Some True
          , security_groups = [ "dlrn-db-centos9" ]
          , boot_from_volume = "yes"
          , volume_size = Some 60
          }
        }
      , Instance::{
        , name = "trunk-server-centos9"
        , groups = [ "dlrn-centos9", "rhel", "rdo" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`4vcpus_8gb`
          , floating_ip = Some True
          , security_groups = [ "web", "rdo-trunk", "apache_exporter" ]
          , boot_from_volume = "yes"
          , volume_size = Some 60
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "trunk-server-centos9-swap"
            , size = 8
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "trunk-builder-centos9"
        , groups = [ "dlrn-centos9", "rdo", "rhel" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , floating_ip = Some True
          , security_groups =
            [ "web", "rdo-trunk", "nfs-trunk-centos9", "apache_exporter" ]
          , boot_from_volume = "yes"
          , volume_size = Some 512
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "trunk-builder-centos9-swap"
            , size = 16
            , device = "/dev/vdb"
            }
          ]
        }
      ]

let extra =
      [ Instance::{
        , name = "backup"
        , groups = [ "backup-server", "rdo", "db_replica" ]
        , connection =
                OS.CentOS.`7.0`.connection
            //  { ansible_host = Some "52.71.149.221"
                , ansible_python_interpreter = "python3"
                }
        }
      , Instance::{
        , name = "trunk-old"
        , connection =
                OS.CentOS.`7.0`.connection
            //  { ansible_host = Some "3.87.151.16", ansible_port = 3300 }
        }
      ]

let vexxhost-instances =
      Infra.Tenant.addSecurityGroupsAndSetFqdn
        [ "common", "monitoring" ]
        fqdn
        (instances # extra)

let ospo-internal-vhosts = [ "rdo-web-builder.int.osci.io" ]

let ospo-external-vhosts = [ "lists.rdoproject.org" ]

let defaultOSPOInternalInstance =
      Instance::{
      , name = "default"
      , groups = [ "osci_zone", "osci_internal_zone", "osci" ]
      , connection = Infra.Connection::{
        , ansible_user = "root"
        , proxy_command = Some "ssh -q rdo@soeru.osci.io -W %h:%p"
        }
      }

let defaultOSPOExternalInstance =
      Instance::{
      , name = "default"
      , groups = [ "osci_zone", "osci" ]
      , node-exporter = False
      , connection = Infra.Connection::{ ansible_user = "root" }
      }

let ospo-instances =
        Instance.textMap
          (Instance.setName defaultOSPOInternalInstance)
          ospo-internal-vhosts
      # Instance.textMap
          (Instance.setName defaultOSPOExternalInstance)
          ospo-external-vhosts

in  vexxhost-instances # ospo-instances # ./ibm-baremetal.dhall
