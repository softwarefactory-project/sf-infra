let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "NodepoolDibImages"
        , rules = Some
          [ Prometheus.AlertingRule::{
            , alert = Some "DibImageOutdated"
            , expr = Some
                "time() - nodepool_dib_image_build_status_last_build > 172800"
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
            , expr = Some "nodepool_dib_image_build_status_rc != 0"
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
              , summary = "Nodepool failed to spawn instance"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "NodepoolLabelNodes"
            , expr = Some "nodepool_label_nodes{state='failed'} > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to spawn instance with label {{ \$labels.label }}"
              }
            }
          , Prometheus.AlertingRule::{
            , alert = Some "NodepoolNodesByProvider"
            , expr = Some "nodepool_provider_nodes{state='failed'} > 0"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = None Text
              , summary =
                  "Nodepool failed to spawn instance on provider {{ \$labels.provider }}"
              }
            }
          ]
        }
      ]
    }
