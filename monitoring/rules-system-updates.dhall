let Prometheus = ./binding.dhall

let unmonitored_instances =
      "instance!~'(trunk.+|www|centos.+|dlrn.*|rpm.*).rdoproject.org:9100'"

let system_reboot =
      { alert = "SystemReboot"
      , name = "system_reboot"
      , summary = "host needs to be rebooted"
      }

let rhel_supported_versions = "8.10|9.2|9.4|9.5|9.6"

let fedora_supported_versions = "41|42"

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
        , name = Some "Info Alerts rules"
        , rules = Some
          [ createRule
              system_reboot.alert
              system_reboot.name
              system_reboot.summary
          , Prometheus.AlertingRule::{
            , alert = Some "RhelEolOsVersions"
            , expr = Some
                "node_os_info{id='rhel', version_id !~ '${rhel_supported_versions}'}"
            , labels = Some Prometheus.infoLabel
            , annotations = Some Prometheus.Annotations::{
              , summary =
                  "System use an outated OS version (instance {{ \$labels.instance }})"
              , description = Some "System use an outated OS version"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "FedoraEolOsVersions"
            , expr = Some
                "node_os_info{id='fedora', version_id !~ '${fedora_supported_versions}'}"
            , labels = Some Prometheus.infoLabel
            , annotations = Some Prometheus.Annotations::{
              , summary =
                  "System use an outated OS version (instance {{ \$labels.instance }})"
              , description = Some "System use an outated OS version"
              }
            }
          ]
        }
      ]
    }
