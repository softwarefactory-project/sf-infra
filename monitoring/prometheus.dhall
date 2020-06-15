let Prometheus = ./binding.dhall

let PrometheusConfig = ./prometheus-config.dhall

let ScrapeConfigs = ./scrape-configs.dhall

let Infra = env:DHALL_INFRA

let {- TODO: Move urls to the Instance objects, like that if the instance is removed,
       the url are also removed from the monitoring
    -} web_monitor_list =
      [ "https://softwarefactory-project.io"
      , "https://softwarefactory-project.io/analytics/elasticsearch/"
      , "https://softwarefactory-project.io/zuul/api/info"
      , "https://softwarefactory-project.io/elasticsearch/"
      , "https://review.rdoproject.org/zuul/api/info"
      , "https://review.rdoproject.org/analytics/app/kibana"
      , "https://review.rdoproject.org"
      , "http://elk.review.rdoproject.org:9200"
      , "https://ovirt.softwarefactory-project.io/zuul/api/info"
      , "https://ansible.softwarefactory-project.io/zuul/api/info"
      , "https://fedora.softwarefactory-project.io/zuul/api/info"
      , "https://www.softwarefactory-project.io"
      , "https://images.rdoproject.org"
      , "https://lists.rdoproject.org"
      , "https://logserver.rdoproject.org"
      , "http://mirror.regionone.rdo-cloud.rdoproject.org"
      , "http://mirror.regionone.vexxhost.rdoproject.org"
      , "https://www.rdoproject.org"
      , "https://trunk.rdoproject.org"
      , "https://trunk.registry.rdoproject.org"
      ]

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
      , ScrapeConfigs.blackbox web_monitor_list
      ,     ScrapeConfigs.static "statsd_exporter" [ "localhost:9102" ]
        //  { scrape_interval = Some "5m" }
      ]
