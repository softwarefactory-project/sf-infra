let Infra = ../../conf/package.dhall

let Common = ../../vars/common.dhall

let security_group_rules =
        Common.web-rules
      # ../../vars/infra-sf/rules/prometheus.dhall
      # ../../vars/infra-sf/rules/push-prox.dhall
      # ../../vars/infra-sf/rules/prometheus-statsd.dhall

let container_images =
    --- NOTE: node_exporter and mysqld_exporter container images are set in group_vars/all
      { prometheus =
          "quay.io/prometheus/prometheus@sha256:788260ebd13613456c168d2eed8290f119f2b6301af2507ff65908d979c66c17"
      , blackbox =
          "quay.io/prometheus/blackbox-exporter@sha256:1d8a5c9ff17e2493a39e4aea706b4ea0c8302ae0dc2aa8b0e9188c5919c9bd9c"
      , alertmanager =
          "quay.io/prometheus/alertmanager@sha256:24a5204b418e8fa0214cfb628486749003b039c279c56b5bddb5b10cd100d926"
      , grafana =
          "docker.io/grafana/grafana@sha256:e86bb674b2c275aae203e6a958c6fad1cb515eb1c1e80c820f4179fc0eb8ea4b"
      , statsd_exporter =
          "quay.io/prometheus/statsd-exporter@sha256:44beed7ad8383328effa0cee14987769dd8137685b75e3297dacbfc5bea103c4"
      }

let firewall_rules =
        Infra.securityGroupRulesToFirewallRules security_group_rules
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
