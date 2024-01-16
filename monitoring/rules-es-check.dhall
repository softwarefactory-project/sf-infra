let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "opensearch-check.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "OpensearchOutdatedMetrics"
            , expr = Some "time() - opensearch_last_update > 259200"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "OpenSearch did not receive any new metrics since 3 days!"
              }
            }
          ]
        }
      ]
    }
