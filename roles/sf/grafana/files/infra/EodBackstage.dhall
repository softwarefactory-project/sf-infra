-- | EOD - Backstage
--
-- Run the following to auto-update on change: watchexec -e dhall "make ./roles/sf/grafana/files/infra/EodBackstage.json && grafdhall ./roles/sf/grafana/files/infra/EodBackstage.dhall"
--
-- Use the following dashboard to get an overview of the platform:
-- <https://monitoring.softwarefactory-project.io/grafana/d/9FFVq15Ik/eod-backstage?orgId=1>
let Panels = ./Panels.dhall

let -- |            Application metrics            | --
    -- The services we maintains:
    appSep =
      Panels.mkSep "Applications"

let -- | Zuul
    -- - What: the zuul jobs results
    -- - Why: check that CI is running
    zuul =
      Panels.mkLucene
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
            Panels.mkPrometheusAppErr
              "Users Requests"
              [ "avg by (job) (increase(http_request[10m]))"
              , "avg by (job) (increase(query{job=\"monocle\"}[10m]))"
              ]
              [ "avg by (job) (increase(http_request_error[10m]))" ]
              ""

      let mem =
            Panels.mkPrometheusApp
              "Runtime Memory Usage"
              [ "avg by (job) (ghc_gcdetails_live_bytes)"
              , "avg by (job) (process_resident_memory_bytes{job=~\"logjuicer|logscraper\"})"
              ]
              "decbytes"

      let cpu =
            Panels.mkPrometheusApp
              "Runtime CPU Usage"
              [ "avg by (job) (rate(ghc_cpu_seconds_total[10m]))"
              , "avg by (job) (rate(process_cpu_seconds_total{job=~\"logjuicer|logscraper\"}[10m]))"
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
      Panels.mkHosts
        [ "zs.softwarefactory-project.io:9100"
        , "managesf.softwarefactory-project.io:9100"
        , "microshift.softwarefactory-project.io:9100"
        , "managesf.review.rdoproject.org:9100"
        ]

let -- | Memory usage
    -- - What: the amount of available memory
    -- - Why: watchout for out of memory errors
    mem =
      Panels.mkPrometheus
        "Available Memory"
        "avg by (instance) (node_memory_MemFree_bytes{instance=~\"${hosts}\"})"
        "decbytes"

let -- | Disk usage
    -- - What: The amount of available disk space clamped
    -- - Why: watchout for out of disk errors
    disk =
      let maxGB = 10

      let maxStr = Natural/show (maxGB * 1024 * 1024 * 1024)

      let mkQuery =
            \(filter : Text) ->
              "clamp_max(avg by(instance) (node_filesystem_avail_bytes{${filter}}), ${maxStr})"

      in  Panels.mkPrometheusMulti
            "Available Disk (max ${Natural/show maxGB} GB)"
            [ mkQuery "instance=~\"${hosts}\", mountpoint=\"/\""
            , mkQuery
                "instance=\"logserver.rdoproject.org:9100\", mountpoint=\"/var/www/logs\""
            , mkQuery
                "instance=\"elk.softwarefactory-project.io:9100\", mountpoint=\"/mnt\""
            ]
            "decbytes"

let -- | Network usage
    -- - What: the amount of data send/recv
    -- - Why: watchout for DDOS
    net =
      Panels.mkPrometheusZeroCentered
        "Network Load (recv are positive, sent are negative)"
        [ "irate(node_network_receive_bytes_total{instance=~\"${hosts}\", device=\"eth0\"}[\$__rate_interval])*8"
        , "irate(node_network_transmit_bytes_total{instance=~\"${hosts}\", device=\"eth0\"}[\$__rate_interval])*(-8)"
        ]
        "bps"

let -- | CPU usage
    -- - What: the load average
    -- - Why: watchout for fork bomb
    cpu =
      Panels.mkPrometheus
        "CPU Load"
        "avg by (instance) (rate(node_cpu_seconds_total{mode=~\"system|user\",instance=~\"${hosts}\"}[1h]))"
        "percentunit"

let dashBoardWIP = Panels.mkDashboard "EOD - BackStage (WIP)" apps

let dashBoard =
      Panels.mkDashboard
        "EOD - BackStage"
        (   [ appSep, zuul ]
          # apps
          # [ Panels.mkSep "Systems", mem, disk, net, cpu ]
        )

in  dashBoard
