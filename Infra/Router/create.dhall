--| The recommanded nomenclature for router

let Router = { Type = ./Type.dhall, default = ./default.dhall }

let create
    : forall (external_network : Text) ->
      forall (network_name : Text) ->
      forall (subnet_name : Text) ->
      forall (router_name : Text) ->
      forall (network_prefix : Text) ->
        Router.Type
    = \(external_network : Text) ->
      \(network_name : Text) ->
      \(subnet_name : Text) ->
      \(router_name : Text) ->
      \(network_prefix : Text) ->
        Router::{
        , name = router_name
        , network = external_network
        , interfaces =
          [ { net = network_name
            , subnet = subnet_name
            , portip = network_prefix ++ ".1"
            }
          ]
        }

let example0 =
        assert
      :     create "public" "my-network" "my-subnet" "my-router" "192.168.0"
        ===  Router::{
             , name = "my-router"
             , network = "public"
             , interfaces =
               [ { net = "my-network"
                 , subnet = "my-subnet"
                 , portip = "192.168.0.1"
                 }
               ]
             }

in  create
