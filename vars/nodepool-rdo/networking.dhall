let Infra = ../../conf/package.dhall

in  { security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          ]
        }
      ]
    , networks = [ Infra.mkNetwork "public" "private" ]
    , subnets = [ Infra.mkSubnet "private" "192.168.1" ]
    , routers = [ Infra.mkRouter "public" "private" "192.168.1" ]
    }
