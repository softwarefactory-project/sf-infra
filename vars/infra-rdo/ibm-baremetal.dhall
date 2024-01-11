let Infra = ../../Infra/package.dhall

let sf_domain = "softwarefactory-project.io"

let rdo_domain = "rdoproject.org"

let mk_instance =
      \(name : Text) ->
      \(groups : List Text) ->
      \(ansible_host : Text) ->
      \(proxy_jump : Text) ->
        Infra.Instance::{
        , name
        , groups
        , node-exporter = False
        , connection = Infra.Connection::{
          , ansible_user = "centos"
          , ansible_host = Some ansible_host
          , ansible_python_interpreter = "python2"
          , proxy_jump = Some proxy_jump
          }
        }

let mk_baremetal =
      \(name : Text) ->
      \(ansible_host : Text) ->
        Infra.Instance::{
        , name
        , groups = [ "baremetal" ]
        , connection = Infra.Connection::{
          , ansible_user = "root"
          , ansible_host = Some ansible_host
          , ansible_python_interpreter = "auto"
          }
        , monitoring_urls_skip_cert_verify = [ "https://${name}" ]
        }

let Cloud =
      { Type =
          { baremetal_name : Text
          , baremetal_ip : Text
          , mirror_ip : Text
          , launcher_name : Text
          , launcher_ip : Text
          , executor_name : Text
          , executor_ip : Text
          , fingergw_name : Text
          , fingergw_ip : Text
          , domain : Text
          }
      , default = {=}
      }

let mk_cloud =
      \(cloud : Cloud.Type) ->
        [ mk_baremetal cloud.baremetal_name cloud.baremetal_ip
        , mk_instance
            ("mirror.regionone." ++ cloud.domain ++ ".rdoproject.org")
            [ "private-afs-mirror" ]
            cloud.mirror_ip
            cloud.baremetal_name
        , mk_instance
            (cloud.launcher_name ++ "." ++ sf_domain)
            [ "sf", "ibm-baremetal-nodepool", "ibm-instance" ]
            cloud.launcher_ip
            cloud.baremetal_name
        , mk_instance
            (cloud.executor_name ++ "." ++ sf_domain)
            [ "sf", "ze", "ibm-instance" ]
            cloud.executor_ip
            cloud.baremetal_name
        , mk_instance
            (cloud.fingergw_name ++ "." ++ sf_domain)
            [ "sf", "ibm-instance" ]
            cloud.fingergw_ip
            cloud.baremetal_name
        ]

let baremetal03 =
      let prefix = "ibm-bm3-"

      in  Cloud::{
          , baremetal_name = "baremetal03." ++ rdo_domain
          , baremetal_ip = "169.60.49.226"
          , mirror_ip = "192.168.25.10"
          , launcher_name = prefix ++ "nodepool-launcher"
          , launcher_ip = "192.168.25.11"
          , executor_name = prefix ++ "ze"
          , executor_ip = "192.168.25.12"
          , fingergw_name = prefix ++ "zfgw"
          , fingergw_ip = "192.168.25.13"
          , domain = prefix ++ "nodepool"
          }

let baremetal04 =
      let prefix = "ibm-bm4-"

      in  Cloud::{
          , baremetal_name = "baremetal04." ++ rdo_domain
          , baremetal_ip = "169.60.49.233"
          , mirror_ip = "192.168.26.10"
          , launcher_name = prefix ++ "nodepool-launcher"
          , launcher_ip = "192.168.26.11"
          , executor_name = prefix ++ "ze"
          , executor_ip = "192.168.26.12"
          , fingergw_name = prefix ++ "zfgw"
          , fingergw_ip = "192.168.26.13"
          , domain = prefix ++ "nodepool"
          }

in  mk_cloud baremetal03 # mk_cloud baremetal04
