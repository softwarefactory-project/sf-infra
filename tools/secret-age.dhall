-- | This file describes the secret rotation policy
let Secret =
      { Type = { match : Text, expiry : Natural, description : Text }
      , default.expiry = 3600 * 24 * 365 * 2
      }

let all =
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
      ]

in  Secret // { all }
