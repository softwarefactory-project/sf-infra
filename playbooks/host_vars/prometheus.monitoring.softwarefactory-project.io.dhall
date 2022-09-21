let Infra = ../../Infra/package.dhall

let Common = ../../vars/common.dhall

let security_group_rules =
        Common.web-rules
      # ../../vars/infra-sf/rules/prometheus.dhall
      # ../../vars/infra-sf/rules/push-prox.dhall
      # ../../vars/infra-sf/rules/prometheus-statsd.dhall

let container_images =
    --- NOTE: node_exporter and mysqld_exporter container images are set in group_vars/all
      { prometheus =
          "quay.io/prometheus/prometheus@sha256:20c90b9a99b12b4349150e347811cc44dccdb05c291d385320be63dc12cce73b"
      , blackbox =
          "quay.io/prometheus/blackbox-exporter@sha256:94de5897eef1b3c1ba7fbfebb9af366e032c0ff915a52c0066ff2e0c1bcd2e45"
      , alertmanager =
          "quay.io/prometheus/alertmanager@sha256:9ab73a421b65b80be072f96a88df756fc5b52a1bc8d983537b8ec5be8b624c5a"
      , grafana =
          "quay.io/software-factory/grafana@sha256:aa6b58d911de35939b065fc2fcd1bed76d3a42f0d7213b47898da7870f0b5c99"
      , statsd_exporter =
          "quay.io/prometheus/statsd-exporter@sha256:8be6604709613409be493520d13a58686ce0b9d69f9b23476195a37379d2df9e"
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
