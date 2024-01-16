let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "backup.rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "BackupTooOld"
            , expr = Some "bup_last_backup{job='node'} < (time() - 259200)"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Backup for {{ \$labels.dir }} has not been updated since {{ \$value | humanizeTimestamp }}"
              }
            }
          ]
        }
      ]
    }
