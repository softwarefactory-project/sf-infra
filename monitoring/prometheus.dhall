let Prometheus = ./binding.dhall

let PrometheusConfig = ./prometheus-config.dhall

let ScrapeConfigs = ./scrape-configs.dhall

let Infra = env:DHALL_INFRA

let {- TODO: maybe move this to an Instance schema
    -} ci_centos_list =
      [ "rdo-ci-cloudslave01.ci.centos.org:9100"
      , "rdo-ci-cloudslave02.ci.centos.org:9100"
      , "rdo-ci-cloudslave03.ci.centos.org:9100"
      , "rdo-ci-cloudslave04.ci.centos.org:9100"
      , "rdo-ci-cloudslave05.ci.centos.org:9100"
      ]

in  PrometheusConfig
      Infra.instances
      [ "rules-node.yaml"
      , "rules-node_proxy.yaml"
      , "rules-http.yaml"
      , "rules-backup.yaml"
      , "rules-dlrn.yaml"
      , "rules-afs.yaml"
      , "rules-systemd.yaml"
      , "rules-nodepool.yaml"
      , "rules-zuul.yaml"
      ]
      [     ScrapeConfigs.static "node_proxy" ci_centos_list
        //  { proxy_url = Some "http://127.0.0.1:8080" }
      , ScrapeConfigs.static
          "journal"
          [ "logreduce-mqtt-01.softwarefactory-project.io:9101" ]
      ,     ScrapeConfigs.static "statsd_exporter" [ "localhost:9102" ]
        //  { scrape_interval = Some "5m" }
      ]
