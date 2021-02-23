let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "elasticsearch-check.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "ElasticsearchOutdatedMetrics"
            , expr = Some "time() - elasticsearch_last_update > 259200"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary =
                  "Elasticsearch did not receive any new metrics since 3 days!"
              }
            }
          ]
        }
      ]
    }
