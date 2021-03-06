let Infra = ../Infra/package.dhall

let vars = ../vars/package.dhall

let Prometheus = ./binding.dhall

let PrometheusConfig = ./prometheus-config.dhall

let ScrapeConfigs = ./scrape-configs.dhall

let {- TODO: maybe move this to an Instance schema
    -} ci_centos_list =
      [ "rdo-ci-cloudslave01.ci.centos.org:9100"
      , "rdo-ci-cloudslave02.ci.centos.org:9100"
      , "rdo-ci-cloudslave03.ci.centos.org:9100"
      , "rdo-ci-cloudslave04.ci.centos.org:9100"
      , "rdo-ci-cloudslave05.ci.centos.org:9100"
      ]

let db_server_list =
      [ "dlrn-db.rdoproject.org:9104", "backup.rdoproject.org:9104" ]

let apache_server_list = [ "trunk.rdoproject.org:9117" ]

let zookeeper_server_list = [ "managesf.softwarefactory-project.io:9141" ]

in  PrometheusConfig
      (Infra.Instance.filter Infra.Instance.getNodeExporter vars.instances)
      [ "rules-node.yaml"
      , "rules-node_proxy.yaml"
      , "rules-http.yaml"
      , "rules-backup.yaml"
      , "rules-dlrn.yaml"
      , "rules-afs.yaml"
      , "rules-systemd.yaml"
      , "rules-nodepool.yaml"
      , "rules-zuul.yaml"
      , "rules-mysqld.yaml"
      , "rules-openstack-check.yaml"
      , "rules-es-check.yaml"
      ]
      [     ScrapeConfigs.static "node_proxy" ci_centos_list
        //  { proxy_url = Some "http://127.0.0.1:8080" }
      ,     ScrapeConfigs.static "statsd_exporter" [ "localhost:9102" ]
        //  { scrape_interval = Some "5m" }
      , ScrapeConfigs.static "mysqld" db_server_list
      , ScrapeConfigs.static "apache" apache_server_list
      , ScrapeConfigs.static "zookeeper" zookeeper_server_list
      ]
