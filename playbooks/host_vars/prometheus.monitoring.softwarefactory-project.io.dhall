let Infra = ../../Infra/package.dhall

let Common = ../../vars/common.dhall

let security_group_rules =
        Common.web-rules
      # ../../vars/infra-sf/rules/prometheus.dhall
      # ../../vars/infra-sf/rules/push-prox.dhall
      # ../../vars/infra-sf/rules/prometheus-statsd.dhall

let container_images =
    --- NOTE: node_exporter and mysqld_exporter container images are set in group_vars/all
    --- prometheus: 2.41.0
    --- blackbox: 0.23.0
    --- alertmanager: 0.25.0
    --- statsd: 0.23.0
      { prometheus =
          "quay.io/prometheus/prometheus@sha256:1a3e9a878e50cd339ae7cf5718fda08381dda2d4ccd28e94bbaa3190d1a566c2"
      , blackbox =
          "quay.io/prometheus/blackbox-exporter@sha256:ca04aa9d90934a2a4d29861d1ebf0e9802e1119ae68690a461e7f6648f6967e2"
      , alertmanager =
          "quay.io/prometheus/alertmanager@sha256:fd4d9a3dd1fd0125108417be21be917f19cc76262347086509a0d43f29b80e98"
      , grafana =
          "quay.io/software-factory/grafana-oss@sha256:1016f0f2612877830ab8f74acbc937fafad9df0b9db1c58c353aea3afd080e40"
      , statsd_exporter =
          "quay.io/prometheus/statsd-exporter@sha256:a9c27602d6f6b86527657922b6a87c12789f7f9b39a90f1513e8c665c941f26a"
      }

let firewall_rules =
        Infra.Rule.map
          Infra.Firewall.Type
          Infra.Firewall.fromRule
          security_group_rules
      # [ { immediate = "yes"
          , permanent = "yes"
          , port = None Text
          , rich_rule = Some
              "rule family=ipv4 source address=192.168.242.246/32 port port=9102 protocol=tcp accept"
          , state = "enabled"
          }
        , { immediate = "yes"
          , permanent = "yes"
          , port = None Text
          , rich_rule = Some
              "rule family=ipv4 source address=38.102.83.250/32 port port=9102 protocol=tcp accept"
          , state = "enabled"
          }
        ]

in  { certbot_plugin = "--apache"
    , firewall_rules
    , fqdn = "prometheus.monitoring.softwarefactory-project.io"
    , grafana_server_root_url = "https://{{ fqdn }}/grafana/"
    , podman_gw_ip = "10.88.0.1"
    , podman_network = "10.88.0.0/16"
    , prometheus_configuration_dir =
        "~/src/softwarefactory-project.io/software-factory/sf-infra/monitoring"
    , prometheus_public_url = "https://{{ fqdn }}/prometheus"
    , pushprox_proxy_listen = ":8080"
    , pushprox_type = "proxy"
    , ssl_cert_options.cert1 =
      { domain = "{{ fqdn }}"
      , email = "softwarefactory-operations-team@redhat.com"
      , webroot = "/var/www/html"
      }
    , udp_mp_dests =
      [ { hostname = "{{ fqdn }}", port = 9125 }
      , { hostname = "elk.softwarefactory-project.io", port = 8125 }
      ]
    , container_images
    }
