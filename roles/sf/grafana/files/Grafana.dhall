-- | A library of functions to create grafana dashboard for prometheus metrics
-- Apply using [grafdhall](https://github.com/softwarefactory-project/grafdhall)
-- or import the json manually: `dhall-to-json --file dashboard.dhall`
--
let Grafana =
      https://raw.githubusercontent.com/weeezes/dhall-grafana/d8c7db7d0900607b60b1f371825bb766de66a940/package.dhall sha256:c79d299d53d930ec82f324d34eaa67bfabc7d90ff41ae881e854302c775462db

let datasource = Some (env:GRAFANA_DATASOURCE as Text ? "prometheus")

let counter =
      \(refId : Text) ->
      \(name : Text) ->
      \(title : Text) ->
        Grafana.MetricsTargets.PrometheusTarget
          Grafana.PrometheusTarget::{
          , refId
          , expr = "increase(${name}[5m])"
          , legendFormat = Some "${title}"
          }

let gauge =
      \(refId : Text) ->
      \(expr : Text) ->
      \(title : Text) ->
        Grafana.MetricsTargets.PrometheusTarget
          Grafana.PrometheusTarget::{
          , refId
          , expr
          , legendFormat = Some "${title}"
          }

let instantgauge =
      \(refId : Text) ->
      \(expr : Text) ->
      \(title : Text) ->
        Grafana.MetricsTargets.PrometheusTarget
          Grafana.PrometheusTarget::{
          , refId
          , expr
          , legendFormat = Some "${title}"
          , instant = True
          }

let panel =
      \(y : Natural) ->
      \(title : Text) ->
      \(targets : List Grafana.MetricsTargets) ->
        Grafana.Panels.mkGraphPanel
          Grafana.GraphPanel::{
          , title
          , gridPos = { x = 0, y, w = 24, h = 6 }
          , datasource
          , targets
          , fill = 0
          , linewidth = 2
          }

let statpanel =
      \(y : Natural) ->
      \(title : Text) ->
      \(targets : List Grafana.MetricsTargets) ->
        Grafana.Panels.mkStatPanel
          Grafana.StatPanel::{
          , title
          , gridPos = { x = 0, y, w = 24, h = 6 }
          , datasource
          , targets
          , options = Grafana.StatPanelOptions::{
            , colorMode = Grafana.StatPanelOptions.ColorMode.value
            }
          }

in  Grafana // { panel, gauge, counter, statpanel, instantgauge }
