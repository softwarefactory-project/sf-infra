let Infra = ../Infra/package.dhall

let vars = ../vars/package.dhall

let Prometheus = ./binding.dhall

let PrometheusConfig = ./prometheus-config.dhall

let ScrapeConfigs = ./scrape-configs.dhall

let db_server_list =
      [ "dlrn-db.rdoproject.org:9104", "backup.rdoproject.org:9104" ]

let apache_server_list =
      [ "trunk.rdoproject.org:9117"
      , "managesf.softwarefactory-project.io:9117"
      , "managesf.review.rdoproject.org:9117"
      , "fedora.softwarefactory-project.io:9117"
      , "mirror.regionone.vexxhost.rdoproject.org:9117"
      ]

let elasticsearch_exporter_list =
      [ "elk.review.rdoproject.org:9114"
      , "opensearch.rdoproject.org:9114"
      , "elk.softwarefactory-project.io:9114"
      ]

let zookeeper_server_list = [ "zk01.softwarefactory-project.io:9141" ]

let -- | Keep in sync with site.yaml and zuul-weeder security group
    zuul-weeder =
      [ "image-builder.softwarefactory-project.io:9001" ]

let logscraper_server_list = [ "logscraper01.openstack.org:9128" ]

let dlrnapi_target_list =
      [ "api-centos9-master-uc"
      , "api-centos9-zed"
      , "api-centos9-yoga"
      , "api-centos9-xena"
      , "api-centos9-wallaby"
      , "api-centos8-yoga"
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
      , "rules-system-package-count-sf.yaml"
      , "rules-system-package-count-rdo.yaml"
      ]
      [     ScrapeConfigs.static "statsd_exporter" [ "localhost:9102" ]
        //  { scrape_interval = Some "5m" }
      , ScrapeConfigs.static "mysqld" db_server_list
      , ScrapeConfigs.static "apache" apache_server_list
      , ScrapeConfigs.static "zookeeper" zookeeper_server_list
      , ScrapeConfigs.dlrn dlrnapi_target_list
      , ScrapeConfigs.static "weeder" zuul-weeder
      , ScrapeConfigs.static "logscraper" logscraper_server_list
      , ScrapeConfigs.static "elasticsearch" elasticsearch_exporter_list
      ]
