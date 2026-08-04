{- This file defines how to import the dhall-prometheus binding
   The DHALL_PROMETHEUS environment variables is used in the CI to use a cached version.
-}
let Prometheus =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-prometheus/5a6a38c65ed85280d28a0f18997a18d1979a2e3a/package.dhall
        sha256:7c1748e1e6af32dba4d14a996d94c5ba5b5b7691a09f1b29e1ab48c406811577

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
