let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "afs.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "AFSTooOld"
            , expr = Some "afs_mirror_sync_time{job='node'} < (time() - 86400)"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary =
                  "Last AFS mirror sync for {{ \$labels.target }} dates back to {{ \$value | humanizeTimestamp }}"
              }
            }
          ]
        }
      ]
    }
