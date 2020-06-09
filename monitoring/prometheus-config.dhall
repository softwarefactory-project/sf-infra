{- This file contains a functions to generate a basic prometheus config -}
let Prometheus = ./binding.dhall

let Infra = ../conf/package.dhall

in      \(instances : List Infra.Instance.Type)
    ->  \(rules : List Text)
    ->  \(targets : List Prometheus.ScrapeConfig.Type)
    ->  let node-target =
                    if Natural/isZero
                         (List/length Infra.Instance.Type instances)

              then  [] : List Prometheus.ScrapeConfig.Type

              else  [ (./scrape-configs.dhall).static
                        "node"
                        ( Infra.Prelude.List.map
                            Infra.Server.Type
                            Text
                            (     \(server : Infra.Server.Type)
                              ->  "${server.name}:9100"
                            )
                            (Infra.getServers instances)
                        )
                    ]

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
                    [ Prometheus.StaticConfig::{
                      , targets = Some [ "localhost:9093" ]
                      }
                    ]
                  }
                ]
              }
            , rule_files = Some rules
            , scrape_configs = Some (node-target # targets)
            }
