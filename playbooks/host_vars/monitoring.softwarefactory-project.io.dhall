let Infra = ../../Infra/package.dhall

let Common = ../../vars/common.dhall

let security_group_rules =
        Common.web-rules
      # ../../vars/infra-sf/rules/prometheus.dhall
      # ../../vars/infra-sf/rules/prometheus-statsd.dhall

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
    , fqdn = "monitoring.softwarefactory-project.io"
    , rhel_release = "9.4"
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
    , routes =
      [ { matchers = [ "severity=~\"warning|info|critical\"", "jobs=~\"hw\"" ]
        , receiver = "rhos-dfg-pcinfra+hwmon"
        }
      , { matchers = [ "severity=\"critical\"" ]
        , receiver = "softwarefactory-operations-team"
        }
      , { matchers = [ "severity=~\"info|warning\"" ], receiver = "blackhole" }
      ]
    }
