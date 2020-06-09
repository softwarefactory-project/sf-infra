let Infra = ../../conf/package.dhall

in  { security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          , Infra.Rule::{ port = +19885 }
          ]
        }
      ]
    , keypairs =
      [ { name = "tripleo-ci-team"
        , public_key = ../files/tripleo_ci_team_key.pub as Text
        }
      ]
    }
