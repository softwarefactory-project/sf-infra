let Infra = ../Infra/package.dhall

let vars = ../vars/package.dhall

let Prometheus = ./binding.dhall

let PrometheusConfig = ./prometheus-config.dhall

let ScrapeConfigs = ./scrape-configs.dhall

let db_server_list =
      [ "dlrn-db.rdoproject.org:9104", "backup.rdoproject.org:9104" ]

let apache_server_list = [ "trunk.rdoproject.org:9117" ]

let zookeeper_server_list = [ "managesf.softwarefactory-project.io:9141" ]

let dlrnapi_target_list =
      [ "api-centos9-master-uc"
      , "api-centos8-master-uc"
      , "api-centos8-xena"
      , "api-centos8-wallaby"
      , "api-centos8-victoria"
      , "api-centos8-ussuri"
      , "api-centos8-train"
      , "api-centos-train"
      ]

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
      [     ScrapeConfigs.static "statsd_exporter" [ "localhost:9102" ]
        //  { scrape_interval = Some "5m" }
      , ScrapeConfigs.static "mysqld" db_server_list
      , ScrapeConfigs.static "apache" apache_server_list
      , ScrapeConfigs.static "zookeeper" zookeeper_server_list
      , ScrapeConfigs.dlrn dlrnapi_target_list
      ]
