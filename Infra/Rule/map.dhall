--| A convenient wrapper for Prelude.List.map pre-applied to Rule
let Prelude = ../Prelude.dhall

let Rule = { Type = ./Type.dhall }

let map
    : forall (type : Type) ->
      forall (f : Rule.Type -> type) ->
      List Rule.Type ->
        List type
    = Prelude.List.map ./Type.dhall

in  map
