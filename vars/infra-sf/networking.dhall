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
              let IP = Text

              let Rule = Infra.Rule.Type

              let {- This function takes an IP as an input and returns a Rule
                  -} pushprox-proxy-access-rule =
                        \(ip : IP)
                    ->  Infra.Rule::{
                        , port = +8080
                        , protocol = Some "tcp"
                        , remote_ip_prefix = Some "${ip}/32"
                        }

              let {- This is a function transformer,
                      it transforms a `IP -> Rule` function to a `List Ip -> List Rule` function
                  -} text-to-rule-map =
                    Infra.Prelude.List.map IP Rule

              let {- The list of IP that can access the pushprox proxy
                  -} pushproxy-clients =
                    [ "8.43.84.199" ]

              in  text-to-rule-map pushprox-proxy-access-rule pushproxy-clients
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
