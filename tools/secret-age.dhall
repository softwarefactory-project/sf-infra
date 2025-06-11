-- | This file describes the secret rotation policy
let day = 3600 * 24

let Secret =
      { Type = { match : Text, expiry : Natural, description : Text }
      , default.expiry = day * 365 * 2
      , day
      }

let Prelude = ../Infra/Prelude.dhall

let Prometheus = ../monitoring/binding.dhall

let secret2rule =
      \(secret : Secret.Type) ->
        let age = "time() - sf_infra_secret_age_total{name=~'${secret.match}'}"

        let two-week = day * 14

        let max-age = "${Natural/show secret.expiry} - ${Natural/show two-week}"

        let desc =
              ''
              ${secret.description}

              See the ansible-vault documentation to perform the actual update.''

        in  Prometheus.AlertingRule::{
            , alert = Some "SecretWillExpireInTwoWeeks"
            , expr = Some "(${age}) > (${max-age})"
            , labels = Some Prometheus.warningLabel
            , annotations = Some
              { description = Some desc
              , summary = "Secret {{ \$labels.target }} will expire in two weeks"
              }
            }

let renderSecretAlerts =
      \(secrets : List Secret.Type) ->
        Prometheus.RulesConfig::{
        , groups = Some
          [ Prometheus.Group::{
            , name = Some "secrets.rules"
            , rules = Some
                ( Prelude.List.map
                    Secret.Type
                    Prometheus.AlertingRule.Type
                    secret2rule
                    secrets
                )
            }
          ]
        }

in  Secret // { renderSecretAlerts }
