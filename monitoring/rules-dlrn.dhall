let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "dlrn.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "RDOTrunkRepoTooOld"
            , expr = Some "dlrn_last_build{job='node'} < (time() - (86400 * 3))"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary =
                  "Last build for {{ \$labels.worker }}/{{ \$labels.symlink }} dates back to {{ \$value | humanizeTimestamp }}"
              }
            }
          ]
        }
      ]
    }
