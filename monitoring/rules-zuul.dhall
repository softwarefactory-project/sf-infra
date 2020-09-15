let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "zuul.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "zuul_blocked_change_total"
            , expr = Some "zuul_blocked_change_total{job='journal'} > 0"
            , for = Some "1h"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary =
                  "The {{ \$labels.tenant }} tenant in Zuul has {{ \$value }} stuck changes"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "zuul_nodepool_requests_failed"
            , expr = Some "increase(zuul_nodepool_requests{state='failed'}[1m])"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary = "Nodepool failed to provide instance to Zuul"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "zuul_nodepool_requests_failed"
            , expr = Some
                "increase(zuul_nodepool_requests_state_by_label{state='failed'}[1m])"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to provide {{ \$labels.label }} to Zuul"
              }
            }
          ]
        }
      ]
    }
