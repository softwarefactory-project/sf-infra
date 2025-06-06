let Prelude = ../Infra/Prelude.dhall

let Prometheus = ./binding.dhall

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

let mkStaticConfig =
      \(target : { name : Text, ip : Text }) ->
      \(subnet : Text) ->
      \(instance : instance.Type) ->
        let port = Text/replace "${subnet}." "" "${instance.ip}"

        in  Prometheus.StaticConfig::{
            , targets = Some [ "${target.name}:91${port}" ]
            , labels = Some
                (Prometheus.Labels.mapText (toMap { instance = instance.name }))
            }

let mkIbmInstancesRules =
      \(target : { name : Text, ip : Text }) ->
      \(subnet : Text) ->
      \(instances : List instance.Type) ->
        Prelude.List.map
          instance.Type
          Prometheus.StaticConfig.Type
          (\(instance : instance.Type) -> mkStaticConfig target subnet instance)
          instances

let baremetal03 = ../vars/infra-rdo/baremetals/baremetal03.dhall

let baremetal04 = ../vars/infra-rdo/baremetals/baremetal04.dhall

in    mkIbmInstancesRules
        baremetal03.baremetal
        baremetal03.subnet
        baremetal03.instances
    # mkIbmInstancesRules
        baremetal04.baremetal
        baremetal04.subnet
        baremetal04.instances
