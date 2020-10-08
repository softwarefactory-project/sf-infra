--| Set the name of an instance
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let setName
    : Instance.Type -> Text -> Instance.Type
    = \(instance : Instance.Type) -> \(name : Text) -> instance // { name }

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     setName
                  Instance::{
                  , name = "old"
                  , connection = Connection::{ ansible_user = "centos" }
                  }
                  "new"
            ===  Instance::{
                 , name = "new"
                 , connection = Connection::{ ansible_user = "centos" }
                 }

in  setName
