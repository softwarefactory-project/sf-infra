let Infra = ../../conf/package.dhall

in      ../nodepool-sf/networking.dhall
    //  { networks =
          [     Infra.mkNetwork "public" "private"
            //  { port_security_enabled = True }
          ]
        , subnets = [ Infra.mkSubnetWithMask "22" "private" "192.168.100" ]
        , keypairs =
          [ { name = "tripleo-ci-team"
            , public_key = ../files/tripleo_ci_team_key.pub as Text
            }
          ]
        }