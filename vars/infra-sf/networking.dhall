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
        , { name = "managesf"
          , rules =
            [ Infra.Rule::{ port = +1883 }
            , Infra.Rule::{ port = +1884 }
            , Infra.Rule::{ port = +29418 }
            , Infra.Rule::{ port = +64738 }
            , Infra.Rule::{ port = +64738, protocol = Some "udp" }
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
