--| Set the name and return the server of an instance
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let Server = ../Server/package.dhall

let getServer
    : Instance.Type -> Optional Server.Type
    = \(instance : ./Type.dhall) ->
        merge
          { None = None Server.Type
          , Some =
              \(server : Server.Type) ->
                Some (server // { name = instance.name })
          }
          instance.server

let example0 =
        assert
      :     getServer
              Instance::{
              , name = "www"
              , server = Some Server::{ image = "centos" }
              }
        ===  Some Server::{ image = "centos", name = "www" }

in  getServer
