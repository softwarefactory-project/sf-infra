-- | EOD - Backstage
--
-- Run the following to auto-update on change: watchexec -e dhall "make ./roles/sf/grafana/files/infra/EodBackstage.json && grafdhall ./roles/sf/grafana/files/infra/EodBackstage.dhall"
--
-- Use the following dashboard to get an overview of the platform:
-- <https://monitoring.softwarefactory-project.io/grafana/d/9FFVq15Ik/eod-backstage?orgId=1>
let Panels = ./Panels.dhall "P1809F7CD0C75ACF3"

let -- |            Application metrics            | --
    -- The services we maintains:
    appSep =
      Panels.mkSep "Applications"

let -- | Zuul
    -- - What: the zuul jobs results
    -- - Why: check that CI is running
    zuul =
      Panels.mkLucene
        "P40D65588D5AED374"
        "Zuul Builds Activity"
        "result.keyword"
        [ { name = "SUCCESS", color = "green" }
        , { name = "TIMED_OUT", color = "purple" }
        ]

let -- | Gerrit
    -- - What: the number of PS creation and replication events
    -- - Why: check the review system is running
    gerrit =
      "TODO"

let -- | Nodepool
    -- - What: the number of node failure per provider
    -- - Why: watchout for external outage
    nodepool =
      "TODO"

let -- | Weeder / Monocle
    -- - What: the service memory usage and query number
    -- - Why: check the service is running
    apps =
      let requests =
            Panels.mkPrometheusErr
              "Users Requests"
              [ { query = "avg by (job) (increase(http_request[10m]))"
                , legend = "{{job}}"
                }
              , { query = "avg by (job) (increase(query{job=\"monocle\"}[10m]))"
                , legend = "{{job}}"
                }
              ]
              [ { query = "avg by (job) (increase(http_request_error[10m]))"
                , legend = "{{job}} error"
                }
              ]
              ""

      let mem =
            Panels.mkPrometheus
              "Runtime Memory Usage"
              [ { query = "avg by (job) (ghc_gcdetails_live_bytes)"
                , legend = "{{job}}"
                }
              , { query =
                    "avg by (job) (process_resident_memory_bytes{job=~\"logjuicer|logscraper\"})"
                , legend = "{{job}}"
                }
              ]
              "decbytes"

      let cpu =
            Panels.mkPrometheus
              "Runtime CPU Usage"
              [ { query = "avg by (job) (rate(ghc_cpu_seconds_total[10m]))"
                , legend = "{{job}}"
                }
              , { query =
                    "avg by (job) (rate(process_cpu_seconds_total{job=~\"logjuicer|logscraper\"}[10m]))"
                , legend = "{{job}}"
                }
              ]
              "percentunit"

      in  [ requests, mem, cpu ]

let -- | NodepoolNodeRequestFulfilledDelay
    -- - What: the average delay a Node request is fulfilled by provider
    -- - Why: watchout for external outage
    nodepoolNodeRequestFulfilled =
      "TODO"

let -- |               System metrics              | --
    -- The main systems we operate (e.g. scheduler, logserver, databases).
    -- To keep the graph digest, replicated hosts like executor/merger are excluded
    hosts =
      [ "zs.softwarefactory-project.io:9100"
      , "managesf.softwarefactory-project.io:9100"
      , "microshift.softwarefactory-project.io:9100"
      , "managesf.review.rdoproject.org:9100"
      ]

let dashBoardWIP = Panels.mkDashboard "EOD - BackStage (WIP)" apps

let dashBoard =
      Panels.mkDashboard
        "EOD - BackStage"
        ([ appSep, zuul ] # apps # Panels.nodeMetrics hosts)

in  dashBoard
