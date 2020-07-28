let Infra = ../../../conf/package.dhall

let statsd-exporter = +9125

let udp-multiplexer = +7000

let metric-emiter = "38.102.83.114/32"

in  Infra.Prelude.List.map
      Integer
      Infra.Rule.Type
      (Infra.udp-ports-rule metric-emiter)
      [ statsd-exporter, udp-multiplexer ]
