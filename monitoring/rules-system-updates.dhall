let Prometheus = ./binding.dhall

let unmonitored_instances =
      "instance!~'(dashboards|trunk.+|www|centos.+|dlrn.*|rpm.*).rdoproject.org:9100'"

let system_package_update =
      { alert = "SystemPackageUpdate"
      , name = "system_package_update"
      , summary = "packages need to be updated"
      }

let system_reboot =
      { alert = "SystemReboot"
      , name = "system_reboot"
      , summary = "host needs to be rebooted"
      }

let createRule =
      \(alert : Text) ->
      \(name : Text) ->
      \(summary : Text) ->
        Prometheus.CriticalRule::{
        , alert = Some alert
        , expr = Some
            "(${name}{instance=~'.*'} and ${name}{${unmonitored_instances}}) != 0"
        , labels = Some Prometheus.infoLabel
        , annotations = Some { description = None Text, summary }
        }

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "system-update rules"
        , rules = Some
          [ createRule
              system_package_update.alert
              system_package_update.name
              system_package_update.summary
          , createRule
              system_reboot.alert
              system_reboot.name
              system_reboot.summary
          ]
        }
      ]
    }
