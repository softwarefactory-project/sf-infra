let Prometheus = ./binding.dhall

let unmonitored_instances =
      "instance!~'((managesf|zs|elk|ansible|zk01|fedora).softwarefactory-project.io|(managesf|logserver).*):9100'"

let rhel_supported_versions =
    {- https://access.redhat.com/support/policy/updates/errata#RHEL8_and_9_Life_Cycle -}
      "8.10|9.2|9.4|9.6|9.7"

let fedora_supported_versions = "42|43"

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "Info Alerts rules"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "RhelEolOsVersions"
            , expr = Some
                "node_os_info{id='rhel', version_id !~ '${rhel_supported_versions}', ${unmonitored_instances}}"
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
