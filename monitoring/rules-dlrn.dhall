let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "dlrn.rules"
        , rules =
            let rule =
                  \(worker : Text) ->
                    Prometheus.AlertingRule::{
                    , alert = Some "RDOTrunkRepoTooOld"
                    , expr = Some "time() - dlrn_last_update{instance=~'${worker}'} > 7200"
                    , labels = Some
                      { severity = "warning"
                      , lasttime = "{{ \$value | humanizeTimestamp }}"
                      }
                    , annotations = Some
                      { description = None Text
                      , summary =
                          "No new activity in {{ \$labels.worker }} for the last two hours"
                      }
                    }

            in  Some
                  [ rule "centos8-.*"
                  , rule "centos9-.*"
                  ]
        }
      ]
    }
