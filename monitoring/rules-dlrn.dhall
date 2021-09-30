let Prometheus = ./binding.dhall

let max-age-c7 = { value = "72h", str = "three days" }

let max-age-c8 = { value = "24h", str = "one day" }

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "dlrn.rules"
        , rules =
            let rule =
                  \(worker : Text) ->
                  \(max-age : { value : Text, str : Text }) ->
                    Prometheus.AlertingRule::{
                    , alert = Some "RDOTrunkRepoTooOld"
                    , expr = Some
                        "increase(dlrn_builds_total{instance=~'${worker}'}[${max-age.value}]) < 1"
                    , labels = Some
                      { severity = "warning"
                      , lasttime = "{{ \$value | humanizeTimestamp }}"
                      }
                    , annotations = Some
                      { description = None Text
                      , summary =
                          "No new builds in {{ \$labels.worker }} for the last ${max-age.str}"
                      }
                    }

            in  Some
                  [ rule "centos-.*" max-age-c7
                  , rule "centos8-.*" max-age-c8
                  , rule "centos9-.*" max-age-c8
                  ]
        }
      ]
    }
