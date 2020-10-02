--| The recommanded nomenclature for router

let Router = { Type = ./Type.dhall, default = ./default.dhall }

let create
    : forall (network : Text) ->
      forall (name : Text) ->
      forall (network_prefix : Text) ->
        Router.Type
    = \(network : Text) ->
      \(name : Text) ->
      \(network_prefix : Text) ->
        Router::{
        , name = name ++ "-router"
        , network
        , interfaces =
          [ { net = name ++ "-network"
            , subnet = name ++ "-subnet"
            , portip = network_prefix ++ ".1"
            }
          ]
        }

let example0 =
        assert
      :     create "public" "mynet" "192.168.0"
        ===  Router::{
             , name = "mynet-router"
             , network = "public"
             , interfaces =
               [ { net = "mynet-network"
                 , subnet = "mynet-subnet"
                 , portip = "192.168.0.1"
                 }
               ]
             }

in  create
