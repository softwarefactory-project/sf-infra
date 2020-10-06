--| The recommanded nomenclature for network

let Network = { Type = ./Type.dhall, default = ./default.dhall }

let create
    : forall (external_network : Text) ->
      forall (network_name : Text) ->
        Network.Type
    = \(external_network : Text) ->
      \(network_name : Text) ->
        { name = network_name, external_network, port_security_enabled = False }

let example0 =
        assert
      :     create "public" "my-network"
        ===  Network::{
             , name = "my-network"
             , external_network = "public"
             , port_security_enabled = False
             }

in  create
