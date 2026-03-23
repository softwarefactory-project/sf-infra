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

in  [] : List Prometheus.StaticConfig.Type
