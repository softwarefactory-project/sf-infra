let Infra = ../../../Infra/package.dhall

let ports-that-prometheus-connect-to =
    --- NOTE: 9100: node_exporter, 9101 mqtt exporter, 9102 statsd_exporter,
    ---       9104 MySQL_exporter
    ---       9114 Elasticsearch Exporter
      [ +9100, +9101, +9102, +9104, +9114 ]

in  Infra.Rule.integerMap
      (\(port : Integer) -> Infra.Rule::{ port, protocol = Some "tcp" })
      ports-that-prometheus-connect-to
