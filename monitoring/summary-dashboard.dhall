let Prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/refs/tags/v17.0.0/Prelude/package.dhall
        sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let partition =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/partition.dhall
        sha256:38147ac6d750a6492736dd90cc967bf09aa405c499de943c64fab7b86ae02f03

let quot =
      \(x : Natural) ->
      \(y : Natural) ->
        let all-lines =
              Prelude.List.generate x Natural (\(x : Natural) -> x * y)

        let lines-within =
              partition
                Natural
                (\(idx : Natural) -> Natural/isZero (Natural/subtract x idx))
                all-lines

        let res = List/length Natural lines-within.true

        in  Natural/subtract 1 res

let lines-count = \(hosts : List Text) -> 1 + quot (List/length Text hosts) 6

let grid-layout =
      \(idx : Natural) ->
        let lines = quot idx 6

        in  { w = 4
            , h = 4
            , x = Natural/subtract (lines * 6) idx * 4
            , y = lines * 4
            }

let gridExample =
        assert
      :     [ grid-layout 0, grid-layout 1, grid-layout 6 ]
        ===  [ { h = 4, w = 4, x = 0, y = 0 }
             , { h = 4, w = 4, x = 4, y = 0 }
             , { h = 4, w = 4, x = 0, y = 4 }
             ]

let -- | This is from https://github.com/weeezes/dhall-grafana/pull/36
    Grafana =
      https://raw.githubusercontent.com/weeezes/dhall-grafana/07371ad984266063615189b07a58204aa76162a5/package.dhall
        sha256:cdd8cf81f25b6bfb2099e2d1732309376e25a6d34139a6d018fc1d01cd0c48c0

let cpuMetric =
      \(host : Text) ->
        ''
        (((count(count(node_cpu_seconds_total{instance="${host}",job="node"}) by (cpu))) - avg(sum by (mode)(rate(node_cpu_seconds_total{mode='idle',instance="${host}",job="node"}[$__range])))) * 100) / count(count(node_cpu_seconds_total{instance="${host}",job="node"}) by (cpu))
        ''

let memMetric =
      \(host : Text) ->
        ''
        100 - ((node_memory_MemAvailable_bytes{instance="${host}",job="node"} * 100) / node_memory_MemTotal_bytes{instance="${host}",job="node"})
        ''

let loadMetric =
      \(host : Text) ->
        ''
        avg(node_load15{instance="${host}",job="node"}) /  count(count(node_cpu_seconds_total{instance="${host}",job="node"}) by (cpu)) * 100
        ''

let datasource = Some "prometheus"

let statpanel =
      \(idx : Natural) ->
      \(startingY : Natural) ->
      \(title : Text) ->
      \(targets : List Grafana.MetricsTargets) ->
        Grafana.Panels.mkStatPanel
          Grafana.StatPanel::{
          , title
          , gridPos = let gp = grid-layout idx in gp // { y = gp.y + startingY }
          , datasource
          , targets
          , fieldConfig = Some
            { overrides = [] : List Grafana.FieldConfig.Override
            , defaults = Some
              { unit = Some "percent"
              , color =
                { fixedColor = "green"
                , mode =
                    < fixed | thresholds | absolute | percentage >.thresholds
                }
              , custom = {=}
              , mappings = [] : List {}
              , thresholds =
                { mode = < fixed | thresholds | absolute | percentage >.absolute
                , steps =
                  [ { color = "rgba(50, 172, 45, 0.97)", value = None Double }
                  , { color = "rgba(237, 129, 40, 0.89)", value = Some 85.0 }
                  , { color = "rgba(245, 54, 54, 0.9)", value = Some 95.0 }
                  ]
                }
              }
            }
          , type = Grafana.StatPanel.PanelType.gauge
          , options = Grafana.StatPanelOptions::{
            , colorMode = Grafana.StatPanelOptions.ColorMode.value
            }
          }

let summaryDashboard =
      \(hosts : List Text) ->
        let HostIX = { startingY : Natural, index : Natural, value : Text }

        let mk-row =
              \(title : Text) ->
              \(mk-host-panel : HostIX -> Natural -> Grafana.Panels.Panels) ->
              \(startingY : Natural) ->
                let row-panels =
                      Prelude.List.map
                        { index : Natural, value : Text }
                        (Natural -> Grafana.Panels.Panels)
                        ( \(host : { index : Natural, value : Text }) ->
                            mk-host-panel (host // { startingY })
                        )
                        (Prelude.List.indexed Text hosts)

                in    [ Grafana.Panels.mkRow
                          Grafana.Row::{
                          , title
                          , gridPos = { x = 0, y = startingY, w = 24, h = 1 }
                          }
                      ]
                    # row-panels

        let -- | Create a simple gauge panel
            mk-gauge-panel =
              \(mkExpr : Text -> Text) ->
              \(host : HostIX) ->
                statpanel
                  host.index
                  host.startingY
                  "${host.value}"
                  [ Grafana.MetricsTargets.PrometheusTarget
                      Grafana.PrometheusTarget::{
                      , refId = host.value
                      , expr = mkExpr host.value
                      }
                  ]

        let ycount = lines-count hosts * 4

        in  Grafana.Dashboard::{
            , title = "WIP Summary Dashboard"
            , uid = Some "all-summary"
            , panels =
                Grafana.Utils.generateIds
                  (   mk-row "CPU" (mk-gauge-panel cpuMetric) 0
                    # mk-row "MEM" (mk-gauge-panel memMetric) ycount
                    # mk-row "LOAD" (mk-gauge-panel loadMetric) (ycount * 2)
                  )
            , editable = True
            }

let targets =
      let Infra = ../Infra/package.dhall

      let instances-raw = ../vars/infra-sf/instances.dhall

      let instances =
            Infra.Instance.filter Infra.Instance.getNodeExporter instances-raw

      in  Infra.Instance.map
            Text
            (\(instance : Infra.Instance.Type) -> "${instance.name}:9100")
            (Infra.Instance.filter Infra.Instance.isReachable instances)

let dash = [ summaryDashboard targets ]

in  dash
