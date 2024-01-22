let Prometheus = ./binding.dhall

let SF =
      { name = "SoftwareFactory"
      , rule =
          ''
          (system_package_update{instance=~".*.softwarefactory-project.io:9100"} and system_package_update{instance!~"(koji|centos).softwarefactory-project.io:9100"}) > 0
          ''
      }

let RDO =
      { name = "RDO"
      , rule =
          ''
          (system_package_update{instance=~".*.(openstack.org|rdoproject.org):9100"} and system_package_update{instance!~"(trunk.+|www|centos.+|dlrn.*|rpm.*).rdoproject.org:9100"}) > 0
          ''
      }

let DLRN =
      { name = "DLRN"
      , rule =
          ''
          system_package_update{instance=~"(trunk.+|centos.+|dlrn.*|rpm.*).rdoproject.org:9100"} > 0
          ''
      }

let OSCI =
      { name = "OSCI"
      , rule =
          ''
          system_package_update{instance="www.rdoproject.org:9100"} > 0
          ''
      }

let createRule =
      \(name : Text) ->
      \(rule : Text) ->
        Prometheus.CriticalRule::{
        , alert = Some "SystemOutdatedPackages${name}"
        , for = Some "7d"
        , expr = Some rule
        , labels = Some Prometheus.infoLabel
        , annotations = Some
          { description = None Text
          , summary = "There are some packages to update in the ${name} infra!"
          }
        }

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "system-package-update-sf.rules"
        , rules = Some
          [ createRule SF.name SF.rule
          , createRule RDO.name RDO.rule
          , createRule OSCI.name OSCI.rule
          ]
        }
      ]
    }
