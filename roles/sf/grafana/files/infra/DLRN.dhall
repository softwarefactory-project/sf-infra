-- | The DLRN grafana dashboard.
let Prelude = ../../../../../Infra/Prelude.dhall

let Grafana = ./Grafana.dhall

let dlrn_worker_list =
      [ "centos9-master-uc"
      , "centos9-xena"
      , "centos9-wallaby"
      , "centos9-yoga"
      , "centos8-yoga"
      , "centos8-xena"
      , "centos8-wallaby"
      , "centos8-victoria"
      , "centos8-ussuri"
      , "centos8-train"
      , "centos-train"
      ]

let mapMetric = Prelude.List.map Text Grafana.MetricsTargets

let mkGaugeMetric =
      \(metric : Text) ->
      \(worker : Text) ->
        Grafana.gauge worker "${metric}{instance=\"${worker}\"}" worker

let mkInstantGaugeMetric =
      \(metric : Text) ->
      \(worker : Text) ->
        Grafana.instantgauge
          worker
          "increase(${metric}{instance=\"${worker}\"}[24h])"
          worker

let graph_panels =
      mapMetric (mkGaugeMetric "dlrn_builds_total") dlrn_worker_list

let total_panels =
      mapMetric (mkInstantGaugeMetric "dlrn_builds_total") dlrn_worker_list

let success_panels =
      mapMetric
        (mkInstantGaugeMetric "dlrn_builds_succeeded_total")
        dlrn_worker_list

let failed_panels =
      mapMetric
        (mkInstantGaugeMetric "dlrn_builds_failed_total")
        dlrn_worker_list

in  Grafana.Dashboard::{
    , title = "RDO Trunk Service"
    , editable = True
    , panels =
        Grafana.Utils.generateIds
          [ Grafana.panel 0 "Total builds by worker" graph_panels
          , Grafana.statpanel 1 "Total builds in the last 24 hours" total_panels
          , Grafana.statpanel
              2
              "Successful builds in the last 24 hours"
              success_panels
          , Grafana.statpanel
              3
              "Failed builds in the last 24 hours"
              failed_panels
          ]
    , links =
      [ Grafana.Link.Type.Link
          Grafana.LinkExternal::{
          , title = "DLRN"
          , url = "https://trunk.rdoproject.org"
          , tooltip = "build service for RDO"
          }
      ]
    }
