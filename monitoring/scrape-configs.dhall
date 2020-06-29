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

in  { static
    , blackbox =
        \(urls : List Text) ->
          Prometheus.ScrapeConfig::{
          , job_name = Some "blackbox"
          , static_configs = Some
            [ Prometheus.StaticConfig::{ targets = Some urls } ]
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
    }
