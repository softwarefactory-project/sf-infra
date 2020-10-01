--| Filter wrapper
let Prelude = ../Prelude.dhall

let Instance = { Type = ./Type.dhall }

let filter
    : (Instance.Type -> Bool) -> List Instance.Type -> List Instance.Type
    = Prelude.List.filter ./Type.dhall

in  filter
