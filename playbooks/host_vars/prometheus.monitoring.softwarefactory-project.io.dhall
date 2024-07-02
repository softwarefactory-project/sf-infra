let Infra = ../../Infra/package.dhall

let Common = ../../vars/common.dhall

let security_group_rules =
        Common.web-rules
      # ../../vars/infra-sf/rules/prometheus.dhall
      # ../../vars/infra-sf/rules/prometheus-statsd.dhall

let container_images =
    --- NOTE: node_exporter and mysqld_exporter container images are set in group_vars/all
    --- prometheus: 2.51.2
    --- blackbox: 0.25.0
    --- alertmanager: 0.27.0
    --- statsd: 0.26.1
    --- grafana: 10.4.2
      { prometheus =
          "quay.io/prometheus/prometheus@sha256:4f6c47e39a9064028766e8c95890ed15690c30f00c4ba14e7ce6ae1ded0295b1"
      , blackbox =
          "quay.io/prometheus/blackbox-exporter@sha256:b04a9fef4fa086a02fc7fcd8dcdbc4b7b35cc30cdee860fdc6a19dd8b208d63e"
      , alertmanager =
          "quay.io/prometheus/alertmanager@sha256:e13b6ed5cb929eeaee733479dce55e10eb3bc2e9c4586c705a4e8da41e5eacf5"
      , grafana =
          "quay.io/software-factory/grafana-oss@sha256:d1c362f245aca3c1c6c7cb2100d38691c41b026aaef60768887135f6323dd890"
      , statsd_exporter =
          "quay.io/prometheus/statsd-exporter@sha256:065c4d566c4ca1f0dacdc4247c2446a2c5514d7c222972445c60d1c08482b005"
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
        , { immediate = "yes"
          , permanent = "yes"
          , port = None Text
          , rich_rule = Some
              "rule family=ipv4 source address=38.102.83.114/32 port port=9125 protocol=udp accept"
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
    , manage_ssh_config = False
    , ssl_cert_options.cert1
      =
      { domain = "{{ fqdn }}"
      , email = "softwarefactory-operations-team@redhat.com"
      , webroot = "/var/www/html"
      }
    , container_images
    }
