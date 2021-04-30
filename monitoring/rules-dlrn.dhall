let Prometheus = ./binding.dhall

let day = "3600 * 24"

let max-age-c7 = { value = "(${day} * 3)", str = "three days" }

let max-age-c8 = { value = "(${day} * 1)", str = "one day" }

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
                        "dlrn_last_build{worker=~'${worker}'} < (time() - ${max-age.value})"
                    , labels = Some
                      { severity = "warning"
                      , lasttime = "{{ \$value | humanizeTimestamp }}"
                      }
                    , annotations = Some
                      { description = None Text
                      , summary =
                          "Last build for {{ \$labels.worker }}/{{ \$labels.symlink }} dates back to {{ \$value | humanizeTimestamp }}, which is older than ${max-age.str} ago"
                      }
                    }

            in  Some
                  [ rule "centos-.*" max-age-c7, rule "centos8-.*" max-age-c8 ]
        }
      ]
    }
