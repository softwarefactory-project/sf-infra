let Infra = ../../conf/infra.dhall

in  Infra.Tenant::{
    , security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          , Infra.Rule::{ port = +19885 }
          ]
        }
      ]
    , networks = [ Infra.mkNetwork "private" ]
    , subnets = [ Infra.mkSubnet "private" "192.168.100" ]
    , routers = [ Infra.mkRouter "private" "192.168.100" ]
    }
