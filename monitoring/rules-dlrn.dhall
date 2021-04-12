let Prometheus = ./binding.dhall

let day = "3600 * 24"

let max-age = "(${day} * 3)"

let max-age-str = "three days"

let max-age-c8 = "(${day} * 1)"

let max-age-str-c8 = "one day"

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "dlrn.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "RDOTrunkRepoTooOld"
            , expr = Some
                "dlrn_last_build{worker=~'centos-.*'} < (time() - ${max-age})"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary =
                  "Last build for {{ \$labels.worker }}/{{ \$labels.symlink }} dates back to {{ \$value | humanizeTimestamp }}, which is older than ${max-age-str} ago"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "RDOTrunkRepoTooOld"
            , expr = Some
                "dlrn_last_build{worker=~'centos8-.*'} < (time() - ${max-age-c8})"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary =
                  "Last build for {{ \$labels.worker }}/{{ \$labels.symlink }} dates back to {{ \$value | humanizeTimestamp }}, which is older than ${max-age-str-c8} ago"
              }
            }
          ]
        }
      ]
    }
