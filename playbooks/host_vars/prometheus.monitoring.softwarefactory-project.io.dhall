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
          "quay.io/prometheus/prometheus@sha256:60190123eb28250f9e013df55b7d58e04e476011911219f5cedac3c73a8b74e6"
      , blackbox =
          "quay.io/prometheus/blackbox-exporter@sha256:1ffc3f109eb39c89df16cdaf359cb9127d4ca2b5313724e939c2c8769aff8176"
      , alertmanager =
          "quay.io/prometheus/alertmanager@sha256:24a5204b418e8fa0214cfb628486749003b039c279c56b5bddb5b10cd100d926"
      , grafana =
          "docker.io/grafana/grafana@sha256:b90de84f06c2fae1fba4e16d8405cd860f50bc769d6e6a54bc991e5d547e4fa3"
      , statsd_exporter =
          "quay.io/prometheus/statsd-exporter@sha256:d23aca343b869352b3391e506748ec741c1c22960fc02cef1c444e9ba083af56"
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
