let Infra = ../../conf/infra.dhall

in  Infra.Tenant::{
    , security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          ]
        }
      ]
    , networks = [ Infra.mkNetwork "private" ]
    , subnets = [ Infra.mkSubnet "private" "192.168.1" ]
    , routers = [ Infra.mkRouter "private" "192.168.1" ]
    }
