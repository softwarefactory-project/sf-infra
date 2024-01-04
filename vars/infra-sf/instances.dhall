let Infra = ../../Infra/package.dhall

let fqdn = "softwarefactory-project.io"

let Instance = Infra.Instance

let OS = (../common.dhall).OS

let Flavors = (../common.dhall).Flavors

let tenant-instance =
      Instance::{
      , name = "tenant"
      , backup = Some Infra.Backup::{ run_sf_backup = True }
      , groups = [ "sf", "backup-sf" ]
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
            , server = Some
                ( Infra.Server.addSecurityGroups
                    [ "elk", "apache_exporter" ]
                    (tenant-server // Infra.Server.Ip "38.102.83.40")
                )
            , volumes =
              [ Infra.Volume::{
                , display_name = "logs-data"
                , size = 100
                , device = "/dev/vdb"
                }
              ]
            }
      ,     tenant-instance
        //  { name = "centos"
            , server = Some
                (     Infra.Server.addSecurityGroups
                        [ "apache_exporter" ]
                        tenant-server
                  //  Infra.Server.Ip "38.102.83.189"
                )
            , volumes =
              [ Infra.Volume::{
                , display_name = "centos-logs-data"
                , size = 250
                , device = "/dev/vdb"
                }
              ]
            }
      ,     tenant-instance
        //  { name = "ansible"
            , server = Some (tenant-server // Infra.Server.Ip "38.102.83.19")
            }
      ]

let instances =
      [ Instance::{ name = "bridge", connection = OS.Fedora.`33`.connection }
      , Instance::{
        , groups = [ "monitoring" ]
        , name = "prometheus.monitoring"
        , connection = OS.CentOS.`7.0`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`7.0`.image.name
          , auto_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups =
            [ "web", "pushprox-proxy", "prometheus", "apache_exporter" ]
          }
        }
      , Instance::{
        , name = "elk"
        , groups = [ "sf" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.CentOS.`7.0`.image.name
                  , flavor = Some Flavors.`4vcpus_16gb`
                  , security_groups = [ "elk" ]
                  }
              //  Infra.Server.Ip "38.102.83.124"
            )
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
        , backup = Some Infra.Backup::{
          , run_sf_backup = True
          , real_name = Some "softwarefactory-project.io"
          }
        , monitoring_urls =
            let note = "TODO: move urls to relevant instance"

            let validate =
                  "curl -o /dev/null -s -w '%{http_code}' URL should return 200"

            in  [ "https://softwarefactory-project.io"
                , "https://softwarefactory-project.io/r/config/server/version"
                , "https://softwarefactory-project.io/zuul/api/info"
                , "https://ansible.softwarefactory-project.io/zuul/api/info"
                , "https://fedora.softwarefactory-project.io/zuul/api/info"
                , "https://www.softwarefactory-project.io"
                , "https://images.rdoproject.org"
                , "https://lists.rdoproject.org"
                , "https://logserver.rdoproject.org"
                , "https://mirror.regionone.vexxhost.rdoproject.org"
                , "https://trunk.rdoproject.org"
                , "https://dashboard.rdo.mtl2.vexxhost.net/auth/login/?next=/"
                ]
        , monitoring_auth_urls =
          [ "https://softwarefactory-project.io/elasticsearch/_cluster/health?wait_for_status=green&timeout=50s"
          , "https://softwarefactory-project.io/elasticsearch/"
          , "https://softwarefactory-project.io/analytics/api/licensing/info"
          ]
        , groups = [ "sf", "install-server", "backup-sf" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.CentOS.`7.0`.image.name
                  , flavor = Some Flavors.`4vcpus_16gb`
                  , boot_from_volume = "yes"
                  , volume_size = Some 20
                  , security_groups = [ "web", "managesf", "apache_exporter" ]
                  }
              //  Infra.Server.Ip "38.102.83.76"
            )
        }
      , Instance::{
        , name = "nodepool-builder"
        , groups = [ "sf", "nodepool-builder", "ibm-baremetal-nodepool" ]
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
        , name = "k1s03"
        , groups =
            let note =
                  "Here we use the k1s group because this host is not managed by sfconfig"

            in  [ "k1s" ]
        , connection = OS.Fedora.`35`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.Fedora.`35`.image.name
                  , network = "oci-private-network"
                  , security_groups = [ "hypervisor-oci" ]
                  , flavor = Some Flavors.`8vcpu_16GB`
                  }
              //  Infra.Server.Ip "38.102.83.139"
            )
        }
      , Instance::{
        , name = "k1s04"
        , groups = [ "k1s" ]
        , connection = OS.Fedora.`38`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.Fedora.`38`.image.name
                  , flavor = Some Flavors.`4vcpus_8gb`
                  , network = "oci-private-network"
                  , security_groups = [ "hypervisor-oci", "cs-k1s" ]
                  }
              //  Infra.Server.Ip "38.102.83.54"
            )
        }
      , Instance::{
        , name = "zk01"
        , groups = [ "sf" ]
        , connection = OS.CentOS.`7.0`.connection
        , server = Some
            (     Infra.Server::{
                  , flavor = Some Flavors.`4vcpus_8gb`
                  , image = OS.CentOS.`7.0`.image.name
                  , security_groups = [ "zookeeper" ]
                  }
              //  Infra.Server.Ip "38.102.83.232"
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
        , backup = Some Infra.Backup::{
          , www_dir = Some "/var/www/html/sfkoji/repos/"
          , playbook = Some "backup-koji.yaml"
          , run_sf_backup = True
          , sf_releases = Some
            [ "sf-master-el7", "sf-3.6-el7-release", "sf-3.5-el7-release" ]
          }
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
        , name = "image-builder"
        , groups = [ "sf" ]
        , connection = OS.CentOS.`8.3`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`8.3`.image.name
          , flavor = Some Flavors.`1vcpu_2gb`
          , auto_ip = Some True
          , security_groups =
            [ "zuul-console"
            , let note =
                    "Keep in sync with site.yaml and prometheus.dhall security group"

              in  "zuul-weeder"
            ]
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "image-builder-data"
            , size = 1000
            , server = "image-builder" ++ "." ++ fqdn
            , device = "/dev/vdb"
            }
          ]
        }
      , Instance::{
        , name = "microshift"
        , groups = [ "sf" ]
        , connection = OS.CentOS.`9-stream`.connection
        , server = Some
            (     Infra.Server::{
                  , image = OS.CentOS.`9-stream`.image.name
                  , flavor = Some Flavors.`8vcpu_16GB`
                  , auto_ip = Some True
                  , boot_from_volume = "yes"
                  , volume_size = Some 100
                  , security_groups = [ "web", "k8s-client" ]
                  }
              //  Infra.Server.Ip "38.102.83.100"
            )
        }
      ]

let -- | A function to create external k1s worker
    mkK1sWorker =
      \(idx : Natural) ->
      \(ip : Text) ->
        Instance::{
        , name = "sf-container-worker-${Natural/show idx}"
        , groups = [ "k1s" ]
        , connection = Infra.Connection::{
          , ansible_user = "sf"
          , ansible_python_interpreter = "auto"
          , ansible_host = Some ip
          }
        }

let -- | These machines are hosted in the ansible org tenant
    -- please contact Goneri Le Bouder <gleboude@redhat.com>
    k1s-workers =
      [ mkK1sWorker 1 "38.129.16.183" ]

let mkServers =
      \(name : Text) ->
      \(flavor : Text) ->
        Infra.Instance.generate
          ( \(idx : Natural) ->
              Instance::{
              , name = "${name}0${Natural/show idx}"
              , groups = [ "sf", name ]
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

let all-instances =
      Infra.Tenant.addSecurityGroupsAndSetFqdn
        default-security-groups
        fqdn
        (instances # tenant-instances # zuuls # k1s-workers)

in  all-instances # ./extra-instances.dhall
