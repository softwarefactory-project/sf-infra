let Secret = ../tools/secret-age.dhall

in  Secret.renderSecretAlerts
      [ Secret::{
        , match = "k1s_*"
        , description =
            ''
            To update the secret:

            - k1s_token: Use `uuidgen`.
            - k1s_key, k1s_crt and k1s_chain: Create a new certifcate.

            After merging the sf-infra change, make sure to restart nodepool-launcher service to update the client.
            ''
        }
      , Secret::{
        , match = "grafana_pass"
        , description =
            ''
            To update the secret use `uuidgen`.
            ''
        }
      , Secret::{
        , match = "zuul_pagure_token_*"
        , description =
            ''
            You need to connect as the zuul user on both pages (see password in bitwarden) and renew the API keys there.
            Restart Zuul with the updated config once the keys have been regenerated.
            ''
        , -- The token are valid for 180 days
          expiry = Secret.day * 180
        }
      , Secret::{
        , match = "zuul_gitlab_com_connection_api_token"
        , description =
            ''
            You need to connect as softwarefactoryio-zuul-bot on gitlab.com (see password in bitwarden) and renew a personal access token.
            Make sure you choose an expiry date of a year when creating the token.
            ''
        , -- The token are valid for 365 days (if the expiry date is properly set)
          expiry = Secret.day * 365
      }
      ]
