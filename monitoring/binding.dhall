{- This file defines how to import the dhall-prometheus binding
   The DHALL_PROMETHEUS environment variables is used in the CI to use a cached version.
-}
let Prometheus =
      https://raw.githubusercontent.com/softwarefactory-project/dhall-prometheus/822a385f0d59ddbfca119f52082c6db61b35eb0e/package.dhall
        sha256:fea80d389b96e1f5565cb88330e27a95fbe1323e87af3d888325c02b335499a0

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
