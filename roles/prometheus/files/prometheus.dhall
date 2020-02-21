let {- Move url definition to the Instance schemas, like that if the instance is removed, the url is removed from the monitoring
    -} web_monitor_list =
      [ "https://softwarefactory-project.io"
      , "https://softwarefactory-project.io/analytics/elasticsearch/"
      , "https://softwarefactory-project.io/zuul/api/info"
      , "https://review.rdoproject.org/zuul/api/info"
      , "https://ovirt.softwarefactory-project.io/zuul/api/info"
      , "https://ansible.softwarefactory-project.io/zuul/api/info"
      , "https://www.softwarefactory-project.io"
      , "https://centos.logs.rdoproject.org"
      , "https://console.registry.rdoproject.org"
      , "https://images.rdoproject.org"
      , "https://lists.rdoproject.org"
      , "https://logs.rdoproject.org"
      , "http://mirror.regionone.rdo-cloud.rdoproject.org"
      , "http://mirror.regionone.vexxhost.rdoproject.org"
      , "https://www.rdoproject.org"
      , "https://review.rdoproject.org"
      , "https://thirdparty.logs.rdoproject.org"
      , "https://trunk.rdoproject.org"
      , "https://trunk.registry.rdoproject.org"
      ]

let host_list =
      let Infra = env:DHALL_INFRA

      in  Infra.mapServerText
            (\(server : Infra.Server.Type) -> server.name ++ ":9100")
            Infra.servers

let RelabelConfig =
      { Type =
          { source_labels : Optional Text
          , target_label : Optional Text
          , replacement : Optional Text
          }
      , default =
          { source_labels = None Text
          , target_label = None Text
          , replacement = None Text
          }
      }

let ScrapeConfig =
      { Type =
          { job_name : Text
          , static_configs : List { targets : List Text }
          , scrape_interval : Optional Text
          , metrics_path : Optional Text
          , params : Optional { module : Text }
          , relabel_configs : Optional (List RelabelConfig.Type)
          }
      , default =
          { scrape_interval = None Text
          , metrics_path = None Text
          , params = None { module : Text }
          , relabel_configs = None (List RelabelConfig.Type)
          }
      }

in  { global =
        { scrape_interval = "1m"
        , scrape_timeout = "10s"
        , evaluation_interval = "1m"
        }
    , alerting.alertmanagers =
      [ { path_prefix = "/alertmanager"
        , static_configs = [ { targets = [ "localhost:9093" ] } ]
        }
      ]
    , rule_files =
      [ "rules.yml"
      , "rules-http.yml"
      , "rules-backup.yml"
      , "rules-dlrn.yml"
      , "rules-afs.yml"
      ]
    , scrape_configs =
      [ ScrapeConfig::{
        , job_name = "node"
        , static_configs = [ { targets = host_list } ]
        }
      , ScrapeConfig::{
        , job_name = "journal"
        , static_configs =
          [ { targets = [ "logreduce-mqtt01.softwarefactory-project.io:9101" ] }
          ]
        }
      , ScrapeConfig::{
        , job_name = "blackbox"
        , static_configs = [ { targets = web_monitor_list } ]
        , scrape_interval = Some "5m"
        , metrics_path = Some "/probe"
        , params = Some { module = "[http_2xx]" }
        , relabel_configs = Some
          [ RelabelConfig::{
            , source_labels = Some "[__address__]"
            , target_label = Some "__param_target"
            }
          , RelabelConfig::{
            , source_labels = Some "[__param_target]"
            , target_label = Some "instance"
            }
          , RelabelConfig::{
            , target_label = Some "__address__"
            , replacement =
                let note = "# Blackbox exporter" in Some "127.0.0.1:9115"
            }
          ]
        }
      ]
    }
