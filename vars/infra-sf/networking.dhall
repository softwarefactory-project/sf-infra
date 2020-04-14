let Infra = ../../conf/package.dhall

let Common = ../common.dhall

let sf_network = { name = "private", network_prefix = "192.168.242" }

let oci_network = { name = "oci-private", network_prefix = "192.168.254" }

let backward-compat-name = { name = "default-router" }

let security_groups =
        Common.SecurityGroups
      # [ { name = "zuul-console", rules = [ Infra.Rule::{ port = +19885 } ] }
        , { name = "monitoring"
          , rules =
            [ Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              , state = Some "absent"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ bridge_private_ip }}/32"
              , state = Some "absent"
              }
            , Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_private_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9100
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            , Infra.Rule::{
              , port = +9101
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "hypervisor-oci"
          , rules =
            [ Infra.Rule::{ port = +19885 }
            , Infra.Rule::{ port = +22022, port_range_max = Some +65535 }
            ]
          }
        , { name = "prometheus-mail"
          , rules =
            [ Infra.Rule::{
              , port = +25
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        , { name = "pushprox-proxy"
          , rules =
              Infra.text-to-rule-map
                (Infra.tcp-access-rule +8080)
                [ "8.43.84.199/32" ]
          }
        , { name = "elk"
          , rules =
            [ Infra.Rule::{ port = +4731 }, Infra.Rule::{ port = +9200 } ]
          }
        ]

in  { networks =
      [ Infra.mkNetwork Common.external-network sf_network.name
      , Infra.mkNetwork Common.external-network oci_network.name
      ]
    , subnets =
      [ Infra.mkSubnet sf_network.name sf_network.network_prefix
      , Infra.mkSubnet oci_network.name oci_network.network_prefix
      ]
    , routers =
      [     Infra.mkRouter
              Common.external-network
              sf_network.name
              sf_network.network_prefix
        //  backward-compat-name
      , Infra.mkRouter
          Common.external-network
          oci_network.name
          oci_network.network_prefix
      ]
    , security_groups = security_groups
    , keypairs =
      [ { name = "sf-infra-key", public_key = Common.sfInfraKeypair } ]
    }
