{- This file defines how to import the dhall-prometheus binding
   The DHALL_PROMETHEUS environment variables is used in the CI to use a cached version.
-}
let Prometheus =
      https://raw.githubusercontent.com/TristanCacqueray/dhall-prometheus/470281423034126e6f7eb532a02fa92684bb9758/package.dhall
        sha256:64144c1a298c0aeeb2591cd30da24e0ab4b15e33269c9174211f73d3076bc983

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
    //  { warningLabel = mkLabel "warning"
        , urgentLabel = mkLabel "urgent"
        , infoLabel = mkLabel "info"
        }
