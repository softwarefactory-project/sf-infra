let Infra = ../../conf/package.dhall

let Common = ../common.dhall

let sf_network = { name = "private", network_prefix = "192.168.242" }

let oci_network = { name = "oci-private", network_prefix = "192.168.254" }

let backward-compat-name = { name = "default-router" }

let monitoring-rules =
      let ports-that-prometheus-connect-to = [ +9100, +9101, +9102 ]

      let private-monitoring-rules =
            Infra.Prelude.List.map
              Integer
              Infra.Rule.Type
              (Infra.tcp-ports-rule "{{ prometheus_private_ip }}/32")
              ports-that-prometheus-connect-to

      let public-monitoring-rules =
            Infra.Prelude.List.map
              Infra.Rule.Type
              Infra.Rule.Type
              (     \(rule : Infra.Rule.Type)
                ->      rule
                    //  { remote_ip_prefix = Some
                            "{{ prometheus_public_ip }}/32"
                        }
              )
              private-monitoring-rules

      in  private-monitoring-rules # public-monitoring-rules

let prometheus-rules =
      let statsd-exporter = +9125

      let udp-multiplexer = +7000

      let metric-emiter = "38.102.83.114/32"

      in  Infra.Prelude.List.map
            Integer
            Infra.Rule.Type
            (Infra.udp-ports-rule metric-emiter)
            [ statsd-exporter, udp-multiplexer ]

let security_groups =
        Common.SecurityGroups
      # [ { name = "zuul-console", rules = [ Infra.Rule::{ port = +19885 } ] }
        , { name = "monitoring", rules = monitoring-rules }
        , { name = "prometheus", rules = prometheus-rules }
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

in  { networks = Some
      [ Infra.mkNetwork Common.external-network sf_network.name
      , Infra.mkNetwork Common.external-network oci_network.name
      ]
    , subnets = Some
      [ Infra.mkSubnet sf_network.name sf_network.network_prefix
      , Infra.mkSubnet oci_network.name oci_network.network_prefix
      ]
    , routers = Some
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
