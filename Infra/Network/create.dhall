--| The recommanded nomenclature for network

let Network = { Type = ./Type.dhall, default = ./default.dhall }

let create
    : forall (network : Text) -> forall (name : Text) -> Network.Type
    = \(network : Text) ->
      \(name : Text) ->
        { name = name ++ "-network"
        , external_network = network
        , port_security_enabled = Some False
        }

let example0 =
        assert
      :     create "public" "mynet"
        ===  Network::{
             , name = "mynet-network"
             , external_network = "public"
             , port_security_enabled = Some False
             }

in  create
