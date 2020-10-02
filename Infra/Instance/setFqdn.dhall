--| Extend the isntance name with an fqdn
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let setFqdn
    : Text -> Instance.Type -> Instance.Type
    = \(fqdn : Text) ->
      \(instance : Instance.Type) ->
        instance // { name = instance.name ++ "." ++ fqdn }

let example0 =
      let Connection = ../Connection/package.dhall

      let Server = ../Server/package.dhall

      in    assert
          :     setFqdn
                  "softwarefactory-project.io"
                  Instance::{
                  , name = "www"
                  , connection = Connection::{ ansible_user = "centos" }
                  , server = Server::{ image = "centos" }
                  }
            ===  Instance::{
                 , name = "www.softwarefactory-project.io"
                 , connection = Connection::{ ansible_user = "centos" }
                 , server = Server::{ image = "centos" }
                 }

in  setFqdn
