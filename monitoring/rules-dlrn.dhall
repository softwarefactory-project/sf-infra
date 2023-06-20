let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "dlrn.rules"
        , rules =
            let rule =
                  \(exp : Text) ->
                  \(time : Text) ->
                  \(hours : Text) ->
                    Prometheus.AlertingRule::{
                    , alert = Some "RDOTrunkRepoTooOld"
                    , expr = Some "time() - dlrn_last_update{${exp}} > ${time}"
                    , labels = Some
                      { severity = "warning"
                      , lasttime = "{{ \$value | humanizeTimestamp }}"
                      }
                    , annotations = Some
                      { description = None Text
                      , summary =
                          "No new activity in {{ \$labels.worker }} for the last ${hours} hours"
                      }
                    }

            in  Some
                  [ rule "index=~'centos8-.*'" "7200" "two"
                  , rule "index=~'centos9-.*', index != 'centos9-master'" "7200" "two"
                  , rule "index='centos9-master'" "25200" "seven"
                  ]
        }
      ]
    }
