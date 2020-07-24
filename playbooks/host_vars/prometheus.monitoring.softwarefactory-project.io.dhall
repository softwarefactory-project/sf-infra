let firewall_rules =
      [ { immediate = "yes"
        , permanent = "yes"
        , port = Some "80/tcp"
        , rich_rule = None Text
        , state = "enabled"
        }
      , { immediate = "yes"
        , permanent = "yes"
        , port = Some "443/tcp"
        , rich_rule = None Text
        , state = "enabled"
        }
      , { immediate = "yes"
        , permanent = "yes"
        , port = Some "9100/tcp"
        , rich_rule = None Text
        , state = "enabled"
        }
      , { immediate = "yes"
        , permanent = "yes"
        , port = Some "9101/tcp"
        , rich_rule = None Text
        , state = "enabled"
        }
      , { immediate = "yes"
        , permanent = "yes"
        , port = None Text
        , rich_rule = Some
            "rule family=ipv4 source address=8.43.84.199/32 port port=8080 protocol=tcp accept"
        , state = "enabled"
        }
      , { immediate = "yes"
        , permanent = "yes"
        , port = None Text
        , rich_rule = Some
            "rule family=ipv4 source address=8.43.83.114/32 port port=7000 protocol=udp accept"
        , state = "enabled"
        }
      , { immediate = "yes"
        , permanent = "yes"
        , port = None Text
        , rich_rule = Some
            "rule family=ipv4 source address=8.43.83.114/32 port port=9125 protocol=udp accept"
        , state = "enabled"
        }
      , { immediate = "yes"
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
    }
