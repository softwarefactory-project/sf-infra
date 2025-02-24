let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "systemd.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "systemd_unit_failed"
            , expr =
                let comment =
                      "instances use name but ibm-instances use exported_name instead"

                in  Some
                      "node_systemd_unit_state{state='failed'} > 0 and (node_systemd_unit_state{name!='dnf-makecache.service'} and node_systemd_unit_state{exported_name!='dnf-makecache.service'})"
            , annotations = Some
              { description = None Text
              , summary =
                  "Instance {{ \$labels.instance }}: Service {{ \$labels.name }} failed"
              }
            }
          , Prometheus.CriticalRule::{
            , alert = Some "systemd_unit_failed"
            , expr =
                let comment =
                      "instances use name but ibm-instances use exported_name instead for name"

                in  Some
                      "node_systemd_unit_state{state='failed'} > 0 and (node_systemd_unit_state{name='dnf-makecache.service'} or node_systemd_unit_state{exported_name='dnf-makecache.service'})"
            , for = Some "1d"
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
