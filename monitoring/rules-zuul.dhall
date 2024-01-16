let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "zuul.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "zuul_blocked_change_total"
            , expr = Some "zuul_blocked_change_total{job='journal'} > 0"
            , for = Some "1h"
            , annotations = Some
              { description = None Text
              , summary =
                  "The {{ \$labels.tenant }} tenant in Zuul has {{ \$value }} stuck changes"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "zuul_nodepool_requests_failed"
            , expr = Some "increase(zuul_nodepool_requests{state='failed'}[1m])"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary = "Nodepool failed to provide instance to Zuul"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "zuul_nodepool_requests_failed"
            , expr = Some
                "increase(zuul_nodepool_requests_state_by_label{state='failed'}[1m])"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to provide {{ \$labels.label }} to Zuul"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "nodepool_high_failure_rate"
            , expr = Some
                "(sum(increase(zuul_nodepool_requests_total{state='failed'}[1h])) or sum(up) * 0) / (sum(increase(zuul_nodepool_requests_total{state='requested'}[1h])) or sum(up) * 0) > 0.1"
            , labels = Some Prometheus.urgentLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "More than 10% of node requests have ended in failure in the last hour. Please check zuul's scheduler logs for NODE_FAILUREs."
              }
            }
          ]
        }
      ]
    }
