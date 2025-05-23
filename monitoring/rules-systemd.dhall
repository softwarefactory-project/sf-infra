let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "systemd.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "systemd_unit_failed"
            , expr = Some
                "node_systemd_unit_state{state='failed', name!='dnf-makecache.service'} > 0"
            , annotations = Some
              { description = None Text
              , summary =
                  "Instance {{ \$labels.instance }}: Service {{ \$labels.name }} failed"
              }
            }
          , Prometheus.CriticalRule::{
            , alert = Some "systemd_unit_flapping"
            , expr = Some
                "changes(node_systemd_unit_state{state='active'}[5m]) > 5"
            , annotations = Some
              { description = None Text
              , summary =
                  "Instance {{ \$labels.instance }}: Service {{ \$labels.name }} is flapping"
              }
            }
          ]
        }
      ]
    }
