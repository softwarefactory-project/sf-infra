let Infra = ../../conf/package.dhall

let Common = ../common.dhall

let rdo_network = { name = "private", network_prefix = "192.168.240" }

let backward-compat-name = { name = "default-router" }

let security_groups =
        Common.SecurityGroups
      # [ { name = "afs"
          , rules =
            [ Infra.Rule::{ port = +8080 }
            , Infra.Rule::{ port = +8081 }
            , Infra.Rule::{ port = +8082 }
            ]
          }
        , { name = "monitoring"
          , rules =
            [ Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ bridge_public_ip }}/32"
              , state = Some "absent"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "rdo-trunk", rules = [ Infra.Rule::{ port = +3300 } ] }
        , { name = "registry"
          , rules =
            [ Infra.Rule::{ port = +53 }
            , Infra.Rule::{ port = +2379 }
            , Infra.Rule::{ port = +2380 }
            , Infra.Rule::{ port = +4001 }
            , Infra.Rule::{ port = +5000 }
            , Infra.Rule::{ port = +8443 }
            , Infra.Rule::{ port = +9090 }
            , Infra.Rule::{ port = +10250 }
            , Infra.Rule::{ port = +4789, protocol = Some "udp" }
            , Infra.Rule::{ port = +8053, protocol = Some "udp" }
            ]
          }
        , { name = "rcn-share"
          , rules =
            [ Infra.Rule::{
              , port = +4433
              , remote_ip_prefix = Some "38.145.32.0/22"
              , protocol = Some "tcp"
              }
            ]
          }
        , { name = "dlrn-db"
          , rules =
              let IP = Text

              let Rule = Infra.Rule.Type

              let {- This function takes an IP as an input and returns a Rule
                  -} dlrn-db-access-rule =
                        \(ip : IP)
                    ->  Infra.Rule::{
                        , port = +3306
                        , protocol = Some "tcp"
                        , remote_ip_prefix = Some "${ip}/32"
                        }

              let {- This is a function transformer,
                      it transforms a `IP -> Rule` function to a `List Ip -> List Rule` function
                  -} text-to-rule-map =
                    Infra.Prelude.List.map IP Rule

              let {- The list of IP that can access the dlrn-db
                  -} dlrn-db-user =
                    [ "54.82.121.165"
                    , "3.87.151.16"
                    , "38.102.83.226"
                    , "38.102.83.175"
                    , "66.187.233.202"
                    ]

              in  text-to-rule-map dlrn-db-access-rule dlrn-db-user
          }
        ]

in  { networks = [ Infra.mkNetwork "public" rdo_network.name ]
    , subnets = [ Infra.mkSubnet rdo_network.name rdo_network.network_prefix ]
    , routers =
      [     Infra.mkRouter "public" rdo_network.name rdo_network.network_prefix
        //  backward-compat-name
      ]
    , security_groups = security_groups
    , keypairs =
      [ { name = "sf-infra-key", public_key = Common.sfInfraKeypair } ]
    }
