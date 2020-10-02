--| Map wrapper
let Prelude = ../Prelude.dhall

let Server = { Type = ./Type.dhall }

let map
    : forall (type : Type) ->
      forall (f : Server.Type -> type) ->
      List Server.Type ->
        List type
    = Prelude.List.map ./Type.dhall

in  map
