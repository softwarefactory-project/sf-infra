{- This file contains function to generate common scrape configuration -}
let Prometheus = ./binding.dhall

let static =
      \(name : Text) ->
      \(targets : List Text) ->
        Prometheus.ScrapeConfig::{
        , job_name = Some name
        , static_configs = Some
          [ Prometheus.StaticConfig::{ targets = Some targets } ]
        }

let dlrn =
      \(targets : List Text) ->
        Prometheus.ScrapeConfig::{
        , job_name = Some "dlrn"
        , static_configs = Some
          [ Prometheus.StaticConfig::{ targets = Some targets } ]
        , scrape_interval = Some "30m"
        , metrics_path = Some "/metrics"
        , relabel_configs = Some
          [ Prometheus.RelabelConfig::{
            , source_labels = Some [ "__address__", "__metrics_path__" ]
            , target_label = Some "__metrics_path__"
            , separator = Some ""
            }
          , Prometheus.RelabelConfig::{
            , source_labels = Some [ "__address__" ]
            , regex = Some "^api-(.*)\$"
            , replacement = Some "\$1"
            , target_label = Some "instance"
            }
          , Prometheus.RelabelConfig::{
            , target_label = Some "__address__"
            , replacement = Some "trunk.rdoproject.org"
            }
          ]
        }

let blackbox-scrape-config =
      \(modules : List Text) ->
      \(urls : List Text) ->
        Prometheus.ScrapeConfig::{
        , job_name = Some "blackbox"
        , static_configs = Some
          [ Prometheus.StaticConfig::{ targets = Some urls } ]
        , scrape_interval = Some "5m"
        , metrics_path = Some "/probe"
        , params = Some Prometheus.Params::{ module = Some modules }
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

in  { static
    , dlrn
    , blackbox = blackbox-scrape-config [ "http_2xx" ]
    , blackbox-auth =
        \(auth-urls : List Text) ->
              blackbox-scrape-config [ "http_4xx" ] auth-urls
          //  { job_name = Some "blackbox-auth" }
    }
