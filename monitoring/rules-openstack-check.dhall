let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "stack-check.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "StackCheckDeleteFailed"
            , expr = Some "stack_delete_failed > 3"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary = "There are at least 3 stacks in deleted_failed state"
              }
            }
          ]
        }
      , Prometheus.Group::{
        , name = Some "neutron-undeleted-ports.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "NeutronUndeletedPorts"
            , expr = Some
                ''
                port_down{is_old="True"} > 3
                ''
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "There are at least 3 ports in down state that are older than 3 days (PORT_TIMEDELTA)"
              }
            }
          ]
        }
      ]
    }
