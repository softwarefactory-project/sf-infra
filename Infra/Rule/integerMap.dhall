--| A convenient wrapper for `Prelude.List.map`. See `./createTcpHost.dhall` for example usage.
let Prelude = ../Prelude.dhall

let Rule = { Type = ./Type.dhall, default = ./default.dhall }

let integerMap
    : forall (f : Integer -> Rule.Type) -> List Integer -> List Rule.Type
    = Prelude.List.map Integer ./Type.dhall

let example0 =
        assert
      :     integerMap (\(port : Integer) -> Rule::{ port }) [ +80, +443 ]
        ===  [ Rule::{ port = +80 }, Rule::{ port = +443 } ]

in  integerMap
