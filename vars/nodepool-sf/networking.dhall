let Infra = ../../Infra/package.dhall

let Common = ../common.dhall

in  { security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          , Infra.Rule::{ port = +19885 }
          ]
        }
      ]
    , networks = Some [ Infra.Network.create "public" "private-network" ]
    , subnets = Some
      [ Infra.Subnet.create
          "private-network"
          "private-subnet"
          "192.168.100"
          Common.dns-servernames
      ]
    , routers = Some
      [ Infra.Router.create
          "public"
          "private-network"
          "private-subnet"
          "private-router"
          "192.168.100"
      ]
    }
