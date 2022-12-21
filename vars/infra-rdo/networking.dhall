let Infra = ../../Infra/package.dhall

let Common = ../common.dhall

let rdo_network = { name = "private", network_prefix = "192.168.240" }

let backward-compat-name = { name = "default-router" }

let security_groups =
        Common.SecurityGroups
      # [ { name = "afs", rules = Common.afs-rules }
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
            , Infra.Rule::{
              , port = +9104
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "rdo-trunk", rules = [ Infra.Rule::{ port = +3300 } ] }
        , { name = "hound", rules = [ Infra.Rule::{ port = +6080 } ] }
        , { name = "elk"
          , rules =
            [ Infra.Rule::{ port = +4731 }
            , Infra.Rule::{ port = +4732 }
            , Infra.Rule::{
              , port = +9200
              , remote_ip_prefix = Some "${rdo_network.network_prefix}.0/24"
              }
            , Infra.Rule::{
              , port = +9200
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9200
              , remote_ip_prefix = Some "38.102.83.141/32"
              }
            , Infra.Rule::{
              , port = +5601
              , remote_ip_prefix = Some "38.102.83.141/32"
              }
            , Infra.Rule::{
              , port = +9114
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "opensearch"
          , rules =
            [ Infra.Rule::{
              , port = +9200
              , remote_ip_prefix = Some "38.102.83.136/32"
              }
            , Infra.Rule::{
              , port = +9200
              , remote_ip_prefix = Some "38.102.83.124/32"
              }
            , Infra.Rule::{
              , port = +9114
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "logstash"
          , rules =
            [ Infra.Rule::{
              , port = +9998
              , remote_ip_prefix = Some "38.102.83.136/32"
              }
            , Infra.Rule::{
              , port = +9997
              , remote_ip_prefix = Some "38.102.83.124/32"
              }
            ]
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
              Infra.Rule.textMap
                (Infra.Rule.createTcpPort +4433)
                [ "38.145.32.0/22", "38.102.83.0/24" ]
          }
        , { name = "dlrn-db"
          , rules =
              Infra.Rule.textMap
                (Infra.Rule.createTcpPort +3306)
                [ "3.87.151.16/32"
                , "38.102.83.226/32"
                , "38.102.83.175/32"
                , "66.187.233.202/32"
                , "52.71.149.221/32"
                ]
          }
        , { name = "mta", rules = [ Infra.Rule::{ port = +25 } ] }
        , { name = "mda"
          , rules = [ Infra.Rule::{ port = +143 }, Infra.Rule::{ port = +993 } ]
          }
        , { name = "apache_exporter"
          , rules =
            [ Infra.Rule::{
              , port = +9117
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        ]

in  { networks = Some [ Infra.Network.create "public" rdo_network.name ]
    , subnets = Some
      [ Infra.Subnet.create
          rdo_network.name
          rdo_network.network_prefix
          Common.dns-servernames
      ]
    , routers = Some
      [     Infra.Router.create
              "public"
              rdo_network.name
              rdo_network.network_prefix
        //  backward-compat-name
      ]
    , security_groups
    , keypairs =
      [ { name = "sf-infra-key", public_key = Common.sfInfraKeypair } ]
    }
