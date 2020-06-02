let Infra = ../../conf/package.dhall

in  { security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          ]
        }
      ]
    , networks = Some [ Infra.mkNetwork "public" "private" ]
    , subnets = Some [ Infra.mkSubnet "private" "192.168.1" ]
    , routers = Some [ Infra.mkRouter "public" "private" "192.168.1" ]
    }
