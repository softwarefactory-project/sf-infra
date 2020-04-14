let Prometheus = ./binding.dhall

let {- TODO: Move urls to the Instance schemas, like that if the instance is removed,
       the url are also removed from the monitoring
    -} web_monitor_list =
      [ "https://softwarefactory-project.io"
      , "https://softwarefactory-project.io/analytics/elasticsearch/"
      , "https://softwarefactory-project.io/zuul/api/info"
      , "http://elk.softwarefactory-project.io:9200"
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

let host_list =
      let Infra = env:DHALL_INFRA

      in  Infra.mapServerText
            (\(server : Infra.Server.Type) -> server.name ++ ":9100")
            Infra.servers

in  Prometheus.Config::{
    , global = Some Prometheus.Global::{
      , scrape_interval = Some "1m"
      , scrape_timeout = Some "10s"
      , evaluation_interval = Some "1m"
      }
    , alerting = Some Prometheus.Alerting::{
      , alertmanagers = Some
        [ Prometheus.Alertmanager::{
          , path_prefix = Some "/alertmanager"
          , static_configs = Some
            [ Prometheus.StaticConfig::{ targets = Some [ "localhost:9093" ] } ]
          }
        ]
      }
    , rule_files = Some
      [ "rules-node.yaml"
      , "rules-node_proxy.yaml"
      , "rules-http.yaml"
      , "rules-backup.yaml"
      , "rules-dlrn.yaml"
      , "rules-afs.yaml"
      ]
    , scrape_configs = Some
      [ Prometheus.ScrapeConfig::{
        , job_name = Some "node"
        , static_configs = Some
          [ Prometheus.StaticConfig::{ targets = Some host_list } ]
        }
      , Prometheus.ScrapeConfig::{
        , job_name = Some "node_proxy"
        , proxy_url = Some "http://127.0.0.1:8080"
        , static_configs = Some
          [ Prometheus.StaticConfig::{ targets = Some ci_centos_list } ]
        }
      , Prometheus.ScrapeConfig::{
        , job_name = Some "journal"
        , static_configs = Some
          [ Prometheus.StaticConfig::{
            , targets = Some
              [ "logreduce-mqtt-01.softwarefactory-project.io:9101" ]
            }
          ]
        }
      , Prometheus.ScrapeConfig::{
        , job_name = Some "blackbox"
        , static_configs = Some
          [ Prometheus.StaticConfig::{ targets = Some web_monitor_list } ]
        , scrape_interval = Some "5m"
        , metrics_path = Some "/probe"
        , params = Some Prometheus.Params::{ module = Some [ "http_2xx" ] }
        , relabel_configs = Some
          [ Prometheus.RelabelConfig::{
            , source_labels = Some [ "__address__" ]
            , target_label = Some "__param_target"
            }
          , Prometheus.RelabelConfig::{
            , source_labels = Some [ "__param_target" ]
            , target_label = Some "instance"
            }
          , Prometheus.RelabelConfig::{
            , target_label = Some "__address__"
            , replacement =
                let note = "# Blackbox exporter" in Some "127.0.0.1:9115"
            }
          ]
        }
      ]
    }
