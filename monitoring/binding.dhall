{- This file defines how to import the dhall-prometheus binding
   The DHALL_PROMETHEUS environment variables is used in the CI to use a cached version.
-}
let Prometheus =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-prometheus/74945e08098dc8125fe125c1759cdf5f88e4fdb2/package.dhall
        sha256:fd646de2e18372de0704d2a8f037cf6abab0fcabff087aa62e7c24f7a6b66883

let -- | Prometheus labels are free form JSON, here is how to define one:
    defaultCriticalLabel
    : Prometheus.Labels.Type
    = Prometheus.Labels.mapText
        ( toMap
            { severity = "critical"
            , lasttime = "{{ \$value | humanizeTimestamp }}"
            }
        )

let -- | An extra schema for critical rules that sends email
    extra =
      { CriticalRule =
        { Type = Prometheus.AlertingRule.Type
        , default =
                Prometheus.AlertingRule.default
            //  { labels = Some defaultCriticalLabel }
        }
      }

let mkLabel =
      \(severity : Text) ->
        Prometheus.Labels.mapText
          (toMap { severity, lasttime = "{{ \$value | humanizeTimestamp }}" })

in      Prometheus
    //  extra
    //  { warningLabel = mkLabel "warning", infoLabel = mkLabel "info" }
