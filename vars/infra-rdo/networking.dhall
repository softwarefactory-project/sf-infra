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
        , { name = "hound", rules = [ Infra.Rule::{ port = +6080 } ] }
        , { name = "elk"
          , rules =
            [ Infra.Rule::{ port = +4731 }, Infra.Rule::{ port = +9200 } ]
          }
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
              Infra.text-to-rule-map
                (Infra.tcp-access-rule +4433)
                [ "38.145.32.0/22", "38.102.83.0/24" ]
          }
        , { name = "dlrn-db"
          , rules =
              Infra.text-to-rule-map
                (Infra.tcp-access-rule +3306)
                [ "54.82.121.165/32"
                , "3.87.151.16/32"
                , "38.102.83.226/32"
                , "38.102.83.175/32"
                , "66.187.233.202/32"
                ]
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
