let Infra = ../../Infra/package.dhall

let Common = ../common.dhall

let sf_network =
      { network_name = "private-network"
      , subnet_name = "private-subnet"
      , router_name = "private-router"
      , network_prefix = "192.168.242"
      }

let oci_network =
      { network_name = "oci-private-network"
      , subnet_name = "oci-private-subnet"
      , router_name = "oci-private-router"
      , network_prefix = "192.168.254"
      }

let backward-compat-name = { name = "default-router" }

let security_groups =
        Common.SecurityGroups
      # [ { name = "zuul-console", rules = [ Infra.Rule::{ port = +19885 } ] }
        , { name = "monitoring", rules = ./rules/monitoring.dhall }
        , { name = "prometheus", rules = ./rules/prometheus-statsd.dhall }
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
        , { name = "pushprox-proxy", rules = ./rules/push-prox.dhall }
        , { name = "elk"
          , rules =
            [ Infra.Rule::{ port = +4731 }
            , Infra.Rule::{
              , port = +9200
              , remote_ip_prefix = Some "${sf_network.network_prefix}.0/24"
              }
            , Infra.Rule::{
              , port = +9200
              , remote_ip_prefix = Some "{{ prometheus_public_ip }}/32"
              }
            ]
          }
        ]

in  { networks = Some
      [ Infra.Network.create Common.external-network sf_network.network_name
      , Infra.Network.create Common.external-network oci_network.network_name
      ]
    , subnets = Some
      [ Infra.Subnet.create
          sf_network.network_name
          sf_network.subnet_name
          sf_network.network_prefix
          Common.dns-servernames
      , Infra.Subnet.create
          oci_network.network_name
          oci_network.subnet_name
          oci_network.network_prefix
          Common.dns-servernames
      ]
    , routers = Some
      [     Infra.Router.create
              Common.external-network
              sf_network.network_name
              sf_network.subnet_name
              sf_network.router_name
              sf_network.network_prefix
        //  backward-compat-name
      , Infra.Router.create
          Common.external-network
          oci_network.network_name
          oci_network.subnet_name
          oci_network.router_name
          oci_network.network_prefix
      ]
    , security_groups
    , keypairs =
      [ { name = "sf-infra-key", public_key = Common.sfInfraKeypair } ]
    }
