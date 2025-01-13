let Infra = ../Infra/package.dhall

let vars = ../vars/package.dhall

let Prometheus = ./binding.dhall

let PrometheusConfig = ./prometheus-config.dhall

let ScrapeConfigs = ./scrape-configs.dhall

let db_server_list = [ "dlrn-db-centos9.rdoproject.org:9104" ]

let apache_server_list =
      [ "managesf.softwarefactory-project.io:9117"
      , "managesf.review.rdoproject.org:9117"
      , "fedora.softwarefactory-project.io:9117"
      , "mirror.regionone.vexxhost.rdoproject.org:9117"
      , "logserver.rdoproject.org:9117"
      , "opensearch.rdoproject.org:9117"
      , "centos.softwarefactory-project.io:9117"
      , "monitoring.softwarefactory-project.io:9117"
      , "trunk-server-centos9.rdoproject.org:9117"
      , "trunk-builder-centos9.rdoproject.org:9117"
      ]

let elasticsearch_exporter_list = [ "opensearch.rdoproject.org:9114" ]

let zookeeper_server_list = [ "zs.softwarefactory-project.io:9141" ]

let logscraper_server_list = [ "logscraper02.openstack.org:9128" ]

let dlrnapi_target_list =
      [ "api-centos9-master-uc"
      , "api-centos9-dalmatian"
      , "api-centos9-caracal"
      , "api-centos9-bobcat"
      , "api-centos9-antelope"
      , "api-centos9-zed"
      , "api-centos9-yoga"
      , "api-centos9-wallaby"
      ]

in  PrometheusConfig
      ( Infra.Instance.keepPresent
          (Infra.Instance.filter Infra.Instance.getNodeExporter vars.instances)
      )
      [ "rules-node.yaml"
      , "rules-ibm-instances.yaml"
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
      , "rules-system-updates.yaml"
      , "rules-k1s-zombie-containers.yaml"
      ]
      [     ScrapeConfigs.static "statsd_exporter" [ "localhost:9102" ]
        //  { scrape_interval = Some "5m" }
      , ScrapeConfigs.static "mysqld" db_server_list
      , ScrapeConfigs.static "apache" apache_server_list
      , ScrapeConfigs.static "zookeeper" zookeeper_server_list
      , ScrapeConfigs.targets
          "ibm-instances"
          ./node-exporter-ibm-instances.dhall
      , ScrapeConfigs.dlrn dlrnapi_target_list
      , ScrapeConfigs.static "logscraper" logscraper_server_list
      , ScrapeConfigs.static "elasticsearch" elasticsearch_exporter_list
      ,     ScrapeConfigs.static "weeder" [ "softwarefactory-project.io" ]
        //  { metrics_path = Some "/weeder/metrics" }
      ,     ScrapeConfigs.static "logjuicer" [ "softwarefactory-project.io" ]
        //  { metrics_path = Some "/logjuicer/metrics" }
      ]
