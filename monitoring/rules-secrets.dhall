let Prelude = ../Infra/Prelude.dhall

let Prometheus = ./binding.dhall

let secrets = ../tools/secret-age.dhall

let secret2rule =
      \(secret : secrets.Type) ->
        let age = "time() - sf_infra_secret_age_total{name='${secret.match}'}"

        let max-age = "${Natural/show secret.expiry} - 604800"

        let desc =
              ''
              ${secret.description}

              See the ansible-vault documentation to perform the actual update.''

        in  Prometheus.AlertingRule::{
            , alert = Some "SecretTooOld"
            , expr = Some "(${age}) > (${max-age})"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = Some desc
              , summary = "Expiring secret {{ \$labels.target }}"
              }
            }

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "secrets.rules"
        , rules = Some
            ( Prelude.List.map
                secrets.Type
                Prometheus.AlertingRule.Type
                secret2rule
                secrets.all
            )
        }
      ]
    }
