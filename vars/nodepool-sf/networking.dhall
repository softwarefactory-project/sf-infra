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
    , networks = Some [ Infra.mkNetwork "public" "private" ]
    , routers = Some [ Infra.mkRouter "public" "private" "192.168.100" ]
    , subnets = Some [ Infra.mkSubnet "private" "192.168.100" ]
    }
