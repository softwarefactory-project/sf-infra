{- This file defines how to import the dhall-prometheus binding
   The DHALL_PROMETHEUS environment variables is used in the CI to use a cached version.
-}
let Prometheus =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-prometheus/0223b72ec9e0cb6fd1cb8140bd3005fd99d383db/package.dhall
        sha256:ff41469ff9239e14781d42c88598f60849751fe9d7108887ad89f3b2b9b8c30c

let -- | Prometheus labels are free form JSON, here is how to define one:
    defaultCriticalLabel
    : Prometheus.Labels.Type
    = Prometheus.Labels.mapText (toMap { severity = "critical" })

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
      \(severity : Text) -> Prometheus.Labels.mapText (toMap { severity })

in      Prometheus
    //  extra
    //  { warningLabel = mkLabel "warning", infoLabel = mkLabel "info" }
