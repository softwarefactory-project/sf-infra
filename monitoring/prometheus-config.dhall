{- This file contains a functions to generate a basic prometheus config -}
let Prometheus = ./binding.dhall

let Prelude = ../Infra/Prelude.dhall

let Infra = ../Infra/package.dhall

let {- Return an empty list if the len is 0
    -} optional-scrape =
      \(len : Natural) ->
      \(scrape : List Prometheus.ScrapeConfig.Type) ->
        if    Natural/isZero len
        then  [] : List Prometheus.ScrapeConfig.Type
        else  scrape

in  \(instances : List Infra.Instance.Type) ->
    \(rules : List Text) ->
    \(targets : List Prometheus.ScrapeConfig.Type) ->
      let node-scrape =
            let node-list =
                  Infra.Instance.map
                    Text
                    ( \(instance : Infra.Instance.Type) ->
                        "${instance.name}:9100"
                    )
                    (Infra.Instance.filter Infra.Instance.isReachable instances)

            in  [ (./scrape-configs.dhall).static "node" node-list ]

      let web-list =
            Prelude.List.concat
              Text
              ( Prelude.List.map
                  Infra.Instance.Type
                  (List Text)
                  ( \(instance : Infra.Instance.Type) ->
                      instance.monitoring_urls
                  )
                  instances
              )

      let web-list-skip-ssl-verify =
            Prelude.List.concat
              Text
              ( Prelude.List.map
                  Infra.Instance.Type
                  (List Text)
                  ( \(instance : Infra.Instance.Type) ->
                      instance.monitoring_urls_skip_cert_verify
                  )
                  instances
              )

      let url-target =
            optional-scrape
              (List/length Text web-list)
              [ (./scrape-configs.dhall).blackbox web-list ]

      let url-target-no-cert-verify =
            optional-scrape
              (List/length Text web-list)
              [ (./scrape-configs.dhall).blackbox-no-cert-verify
                  web-list-skip-ssl-verify
              ]

      let auth-web-list =
            Prelude.List.concat
              Text
              ( Prelude.List.map
                  Infra.Instance.Type
                  (List Text)
                  ( \(instance : Infra.Instance.Type) ->
                      instance.monitoring_auth_urls
                  )
                  instances
              )

      let auth-url-target =
            optional-scrape
              (List/length Text auth-web-list)
              [ (./scrape-configs.dhall).blackbox-auth auth-web-list ]

      let node-target =
            optional-scrape
              (List/length Infra.Instance.Type instances)
              node-scrape

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
          , scrape_configs = Some
              (   node-target
                # url-target
                # url-target-no-cert-verify
                # auth-url-target
                # targets
              )
          }
