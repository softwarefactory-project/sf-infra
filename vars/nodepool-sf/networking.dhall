let Infra = ../../Infra/package.dhall

let Common = ../common.dhall

in  { security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          , Infra.Rule::{ port = +19885 }
          , Infra.Rule::{
            , port = +5001
            , remote_ip_prefix = Some "38.129.56.0/24"
            }
          , Infra.Rule::{
            , port = +5001
            , remote_ip_prefix = Some "38.102.83.0/24"
            }
          ]
        }
      ]
    , networks = Some [ Infra.Network.create "public" "private" ]
    , routers = Some [ Infra.Router.create "public" "private" "192.168.100" ]
    , subnets = Some
      [ Infra.Subnet.create "private" "192.168.100" Common.dns-servernames ]
    }
