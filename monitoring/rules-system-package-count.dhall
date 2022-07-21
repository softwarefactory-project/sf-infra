let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "system-package-update.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "SystemOutdatedPackages"
            , expr = Some "system_package_update > 1"
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary = "There are some packages to update!"
              }
            }
          ]
        }
      ]
    }
