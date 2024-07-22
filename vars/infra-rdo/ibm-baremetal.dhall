let Infra = ../../Infra/package.dhall

let Prelude = ../../Infra/Prelude.dhall

let instance =
      { Type =
          { name : Text
          , ip : Text
          , groups : List Text
          , proxy_jump : Optional Text
          , ansible_user : Text
          }
      , default = {}
      }

let mkInstances =
      \(instances : List instance.Type) ->
        Prelude.List.map
          instance.Type
          Infra.Instance.Type
          ( \(instance : instance.Type) ->
              Infra.Instance::{
              , name = instance.name
              , groups = instance.groups
              , node-exporter = False
              , connection = Infra.Connection::{
                , ansible_user = Some instance.ansible_user
                , ansible_host = Some instance.ip
                , ansible_python_interpreter = "auto"
                , proxy_jump = instance.proxy_jump
                }
              }
          )
          instances

let mkBaremetal =
      \(baremetal : { name : Text, ip : Text }) ->
        Infra.Instance::{
        , name = baremetal.name
        , groups = [ "baremetal" ]
        , connection = Infra.Connection::{
          , ansible_user = Some "root"
          , ansible_host = Some baremetal.ip
          , ansible_python_interpreter = "auto"
          }
        , monitoring_urls_skip_cert_verify = [ "https://${baremetal.name}" ]
        }

in    [ mkBaremetal (./baremetals/baremetal03.dhall).baremetal ]
    # mkInstances (./baremetals/baremetal03.dhall).instances
    # [ mkBaremetal (./baremetals/baremetal04.dhall).baremetal ]
    # mkInstances (./baremetals/baremetal04.dhall).instances
