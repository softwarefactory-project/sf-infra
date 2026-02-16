let Infra = ../../Infra/package.dhall

let fqdn = "softwarefactory-project.io"

let Instance = Infra.Instance

let OS = (../common.dhall).OS

let Flavors = (../common.dhall).Flavors

let tenant-rhel-9-instance =
      Instance::{
      , name = "tenant"
      , groups = [ "rhel", "sf", "backup-sf" ]
      , connection = OS.RHEL.`9.3`.connection
      }

let mkMicroshiftZe =
      \(id : Text) ->
        Instance::{
        , name = "microshift-ze0${id}"
        , groups =
          [ "sf-operator", "centos-infra-zuul-executors", "rhel", "promtail" ]
        , connection = OS.RHEL.`9.4`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.4`.image.name
          , flavor = Some Flavors.`8vcpu_16GB`
          , network = "public"
          , boot_from_volume = "yes"
          , volume_size = Some 100
          , security_groups = [ "k8s-client", "zuul-finger" ]
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "ze0${id}-lvm"
            , size = 512
            , device = "/dev/vdb"
            }
          ]
        }

let tenant-rhel-9-server =
      Infra.Server::{
      , image = OS.RHEL.`9.3`.image.name
      , floating_ip = Some True
      , volume_size = Some 40
      , security_groups = [ "web" ]
      }

let tenant-instances =
      [     tenant-rhel-9-instance
        //  { name = "fedora"
            , backup = Some Infra.Backup::{
              , run_sf_backup = True
              , dir = Some
                  "/var/lib/backup/bup/fedora.softwarefactory-project.io"
              , domain = Some "fedora.softwarefactory-project.io"
              , month_subdir = Some 1
              }
            , server = Some
                ( Infra.Server.addSecurityGroups
                    [ "elk", "apache_exporter" ]
                    tenant-rhel-9-server
                )
            , volumes =
              [ Infra.Volume::{
                , display_name = "logs-data"
                , size = 100
                , device = "/dev/vdb"
                }
              ]
            }
      ,     tenant-rhel-9-instance
        //  { name = "ansible"
            , backup = Some Infra.Backup::{
              , run_sf_backup = True
              , dir = Some
                  "/var/lib/backup/bup/ansible.softwarefactory-project.io"
              , domain = Some "ansible.softwarefactory-project.io"
              , month_subdir = Some 1
              }
            , server = Some
                ( Infra.Server.addSecurityGroups
                    [ "elk", "apache_exporter" ]
                    tenant-rhel-9-server
                )
            }
      ]

let -- | A function to create k1s hosts
    mkK1sHost =
      \(idx : Natural) ->
      \(flavor : Text) ->
      \(security_groups : List Text) ->
        Instance::{
        , name = "k1s0${Natural/show idx}"
        , groups = [ "epel", "k1s-secrets", "k1s-rhel", "rhel", "promtail" ]
        , connection = OS.RHEL.`9.4`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.4`.image.name
          , network = "oci-private-network"
          , floating_ip = Some True
          , security_groups
          , flavor = Some flavor
          , boot_from_volume = "yes"
          , volume_size = Some 100
          }
        }

let instances =
      [ Instance::{
        , name = "bridge"
        , groups =
          [ "bridge"
          , "k1s-secrets"
          , "observability-stack"
          , "promtail"
          , "rhel"
          , "sf-operator"
          ]
        , connection = OS.RHEL.`10.1`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`10.1`.image.name
          , floating_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 40
          }
        }
      , Instance::{
        , groups = [ "monitoring", "rhel" ]
        , name = "monitoring"
        , connection = OS.RHEL.`9.4`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.4`.image.name
          , floating_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 80
          , security_groups = [ "web", "prometheus", "apache_exporter" ]
          }
        }
      , Instance::{
        , name = "elk"
        , groups = [ "rhel", "sf", "promtail" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , floating_ip = Some True
          , security_groups = [ "elk" ]
          , volume_size = Some 50
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
        , backup = Some Infra.Backup::{
          , run_sf_backup = True
          , real_name = Some "softwarefactory-project.io"
          , dir = Some "/var/lib/backup/bup/softwarefactory-project.io"
          , domain = Some "softwarefactory-project.io"
          , month_subdir = Some 1
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
                , "https://lists.rdoproject.org"
                , "https://logserver.rdoproject.org"
                , "https://mirror.regionone.vexxhost.rdoproject.org"
                , "https://trunk.rdoproject.org"
                , "https://dashboard.rdo.mtl2.vexxhost.net/auth/login/?next=/"
                ]
        , groups = [ "rhel", "sf", "install-server", "backup-sf", "promtail" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , boot_from_volume = "yes"
          , floating_ip = Some True
          , volume_size = Some 100
          , security_groups = [ "web", "managesf", "apache_exporter" ]
          }
        }
      , Instance::{
        , name = "managesf-dev"
        , groups = [ "rhel", "sf", "install-server" ]
        , connection = OS.RHEL.`9.4`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.4`.image.name
          , flavor = Some Flavors.`4vcpus_16gb`
          , boot_from_volume = "yes"
          , network = "public"
          , volume_size = Some 100
          , security_groups = [ "web", "managesf", "apache_exporter" ]
          }
        }
      , Instance::{
        , name = "nodepool-builder"
        , groups =
          [ "sf"
          , "rhel"
          , "nodepool-builder"
          , "ibm-baremetal-nodepool"
          , "promtail"
          ]
        , connection = OS.RHEL.`9.4`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.4`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , boot_from_volume = "yes"
          , volume_size = Some 50
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "nodepool_builder_lib"
            , size = 400
            , device = "/dev/vdb"
            }
          , Infra.Volume::{
            , display_name = "nodepool_builder_cache"
            , size = 200
            , device = "/dev/vdc"
            }
          ]
        }
      , mkK1sHost 3 Flavors.`8vcpu_16GB` [ "hypervisor-oci" ]
      , mkK1sHost 4 Flavors.`4vcpus_8gb` [ "hypervisor-oci", "cs-k1s" ]
      , mkK1sHost 5 Flavors.`8vcpu_16GB` [ "hypervisor-oci" ]
      , mkK1sHost 6 Flavors.`8vcpu_16GB` [ "hypervisor-oci-open-k1s" ]
      , Instance::{
        , name = "zk01"
        , groups = [ "rhel", "sf", "promtail" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , flavor = Some Flavors.`4vcpus_8gb`
          , image = OS.RHEL.`9.3`.image.name
          , floating_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 50
          , security_groups = [ "zookeeper" ]
          }
        }
      , Instance::{
        , name = "zs"
        , groups = [ "rhel", "sf", "promtail" ]
        , connection = OS.RHEL.`9.3`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.3`.image.name
          , flavor = Some Flavors.`2vcpus_8gb`
          , boot_from_volume = "yes"
          , volume_size = Some 50
          }
        }
      , Instance::{
        , name = "image-builder"
        , groups = [ "sf", "rhel", "promtail" ]
        , connection = OS.RHEL.`9.4`.connection
        , server = Some Infra.Server::{
          , image = OS.RHEL.`9.4`.image.name
          , flavor = Some Flavors.`1vcpu_2gb`
          , floating_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 50
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
      , mkMicroshiftZe "1"
      , mkMicroshiftZe "2"
      , Instance::{
        , name = "microshift-infra"
        , groups = [ "promtail", "promtail-openshift" ]
        , connection = OS.CentOS.`9-stream`.connection
        , server = Some Infra.Server::{
          , image = OS.CentOS.`9-stream`.image.name
          , flavor = Some Flavors.`8vcpu_16GB`
          , floating_ip = Some True
          , boot_from_volume = "yes"
          , volume_size = Some 100
          , security_groups = [ "web", "k8s-client" ]
          }
        , volumes =
          [ Infra.Volume::{
            , display_name = "microshift-infra-data"
            , size = 200
            , device = "/dev/vdb"
            }
          ]
        }
      ]

let -- | A function to create external k1s worker
    mkK1sWorker =
      \(idx : Natural) ->
      \(ip : Text) ->
        Instance::{
        , name = "sf-container-worker-${Natural/show idx}"
        , groups = [ "epel", "k1s-secrets", "k1s-rhel", "rhel", "promtail" ]
        , connection = Infra.Connection::{
          , ansible_user = Some "cloud-user"
          , ansible_python_interpreter = "auto"
          , ansible_host = Some ip
          }
        }

let -- | These machines are hosted in the ansible org tenant
    -- please contact jrouleau@redhat.com or mgraves@redhat.com
    k1s-workers =
      [ mkK1sWorker 1 "38.129.16.117" ]

let mkServers =
      \(name : Text) ->
      \(flavor : Text) ->
        Infra.Instance.generate
          ( \(idx : Natural) ->
              Instance::{
              , name = "${name}0${Natural/show idx}"
              , groups = [ "rhel", "sf", name, "promtail" ]
              , connection = OS.RHEL.`9.4`.connection
              , server = Some Infra.Server::{
                , image = OS.RHEL.`9.4`.image.name
                , flavor = Some flavor
                , boot_from_volume = "no"
                }
              }
          )

let mkExecutors = mkServers "ze" Flavors.`4vcpus_8gb`

let mkMergers = mkServers "zm" Flavors.`1vcpu_4gb`

let zuuls = mkExecutors 4 # mkMergers 4

let default-security-groups = [ "common", "monitoring", "internal" ]

let all-instances =
      Infra.Tenant.addSecurityGroupsAndSetFqdn
        default-security-groups
        fqdn
        (instances # tenant-instances # zuuls # k1s-workers)

in  all-instances # ./extra-instances.dhall
