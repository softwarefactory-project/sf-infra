let Infra = ../../../Infra/package.dhall

let ports-that-prometheus-connect-to = [ +9100, +9101, +9102, +9104 ]

in  Infra.Rule.integerMap
      (\(port : Integer) -> Infra.Rule::{ port, protocol = Some "tcp" })
      ports-that-prometheus-connect-to
