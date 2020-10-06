--| Map wrapper
let Instance =
      { Type = ./Type.dhall
      , default = ./default.dhall
      , getName = ./getName.dhall
      }

let Prelude = ../Prelude.dhall

let map
    : forall (type : Type) ->
      forall (f : Instance.Type -> type) ->
      List Instance.Type ->
        List type
    = Prelude.List.map Instance.Type

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     map
                  Text
                  Instance.getName
                  [ Instance::{
                    , name = "www"
                    , connection = Connection::{ ansible_user = "centos" }
                    }
                  ]
            ===  [ "www" ]

in  map
