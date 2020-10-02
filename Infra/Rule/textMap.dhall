--| A convenient wrapper for `Prelude.List.map`. See `./createTcpPort.dhall` for example usage.
let Prelude = ../Prelude.dhall

let Rule = { Type = ./Type.dhall }

let textMap
    : forall (f : Text -> Rule.Type) -> List Text -> List Rule.Type
    = Prelude.List.map Text ./Type.dhall

in  textMap
