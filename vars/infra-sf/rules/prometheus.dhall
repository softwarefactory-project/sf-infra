let Infra = ../../../conf/package.dhall

let ports-that-prometheus-connect-to = [ +9100, +9101, +9102 ]

in  Infra.Prelude.List.map
      Integer
      Infra.Rule.Type
      (\(port : Integer) -> Infra.Rule::{ port, protocol = Some "tcp" })
      ports-that-prometheus-connect-to
