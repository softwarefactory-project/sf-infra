--| A convenient wrapper for `Prelude.List.map`
let Prelude = ../Prelude.dhall

let Instance = { Type = ./Type.dhall }

let textMap
    : forall (f : Text -> Instance.Type) -> List Text -> List Instance.Type
    = Prelude.List.map Text Instance.Type

in  textMap
