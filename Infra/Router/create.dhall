--| The recommanded nomenclature for router
let Router =
      { Type = ./Type.dhall
      , default = ./default.dhall
      , Interfaces = ./Interfaces.dhall
      }

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
          [ Router.Interfaces.full
              { net = name ++ "-network"
              , subnet = Some (name ++ "-subnet")
              , portip = Some (network_prefix ++ ".1")
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
               [ Router.Interfaces.full
                   { net = "mynet-network"
                   , subnet = Some "mynet-subnet"
                   , portip = Some "192.168.0.1"
                   }
               ]
             }

in  create
