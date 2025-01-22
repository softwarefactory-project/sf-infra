let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "NodepoolDibImages"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "DibImageOutdated"
            , expr = Some
                "time() - nodepool_dib_image_status_last_build > 172800"
            , for = Some "24h"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool DIB image {{ \$labels.image }} was last built {{ \$labels.lasttime }}!"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "DibImageBuildFailed"
            , expr = Some "nodepool_dib_image_status_rc != 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary = "DIB image {{ \$labels.image }} build failed"
              }
            }
          ]
        }
      , Prometheus.Group::{
        , name = Some "NodepoolLauncher"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "NodepoolNodes"
            , expr = Some "nodepool_nodes{state='failed'} > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to spawn instance in {{ \$labels.provider }}"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "NodepoolNodesByLabel"
            , expr = Some "nodepool_nodes_by_label{state='failed'} > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to spawn instance with label {{ \$labels.label }}"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "NodepoolNodesByProvider"
            , expr = Some "nodepool_nodes_by_provider{state='failed'} > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to spawn instance on provider {{ \$labels.provider }}"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "NodepoolLaunchByProvider"
            , expr = Some
                "increase(nodepool_launch_by_provider{result!='ready'}[1m]) > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to spawn instance with label {{ \$labels.label }} on {{ \$labels.provider }}"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "NodepoolLaunchByRequestor"
            , expr = Some
                "increase(nodepool_launch_by_requestor{result!='ready'}[1m]) > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to launch instance for requestor {{ \$labels.requestor }}"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "NodepoolLaunch"
            , expr = Some "increase(nodepool_launch{result!='ready'}[1m]) > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to launch instance with error {{ \$labels.result }}"
              }
            }
          ]
        }
      ]
    }
