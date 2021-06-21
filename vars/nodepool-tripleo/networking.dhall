let Infra = ../../Infra/package.dhall

in  { security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          , Infra.Rule::{ port = +19885 }
          , Infra.Rule::{
            , port = +5001
            , remote_ip_prefix = Some "38.102.83.0/24"
            }
          , Infra.Rule::{
            , port = +8766
            , remote_ip_prefix = Some "38.102.83.0/24"
            }
          ]
        }
      ]
    , keypairs =
      [ { name = "tripleo-ci-team"
        , public_key = ../files/tripleo_ci_team_key.pub as Text
        }
      ]
    }
