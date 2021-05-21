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
          "quay.io/prometheus/prometheus@sha256:5accb68b56ba452e449a5e552411acaeabbbe0f087acf19a1157ce3dd10a8bed"
      , blackbox =
          "quay.io/prometheus/blackbox-exporter@sha256:94de5897eef1b3c1ba7fbfebb9af366e032c0ff915a52c0066ff2e0c1bcd2e45"
      , alertmanager =
          "quay.io/prometheus/alertmanager@sha256:24a5204b418e8fa0214cfb628486749003b039c279c56b5bddb5b10cd100d926"
      , grafana =
          "quay.io/software-factory/grafana@sha256:91ebc00a7427cd6156d0c2b51ba243dfb4659d1f8c8cee11e37b735798606ae4"
      , statsd_exporter =
          "quay.io/prometheus/statsd-exporter@sha256:32e64e411fc1df9329aafed42b1b48740837bc84f116350d8e9feab8bd64c27c"
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
