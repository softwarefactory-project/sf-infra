let Prometheus = ./binding.dhall

let K1S =
      { rule =
          ''
          k1s_old_containers > 0
          ''
      }

let createRule =
      \(rule : Text) ->
        Prometheus.CriticalRule::{
        , alert = Some "K1SZombieContainers"
        , for = Some "3d"
        , expr = Some rule
        , labels = Some Prometheus.infoLabel
        , annotations = Some
          { description = None Text
          , summary = "There are some zombie containers available on k1s host!"
          }
        }

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "k1s-zombie-containers.rules"
        , rules = Some [ createRule K1S.rule ]
        }
      ]
    }
