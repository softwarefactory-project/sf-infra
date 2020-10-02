--| Set the name and return the server of an instance
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let Server = ../Server/package.dhall

let getServer
    : Instance.Type -> Server.Type
    = \(instance : ./Type.dhall) -> instance.server // { name = instance.name }

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     getServer
                  Instance::{
                  , name = "www"
                  , connection = Connection::{ ansible_user = "centos" }
                  , server = Server::{ image = "centos" }
                  }
            ===  Server::{ image = "centos", name = "www" }

in  getServer
