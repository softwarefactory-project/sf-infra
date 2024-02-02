-- | To generate a new panel, copy the panel JSON from the UI.
-- Use `json-to-dhall | dhall type` to generate the type and add it to the 'PType' union.
-- Then use `json-to-dhall` to create a mkHelper, moving the common values as function parameters.
--
let -- | The default graph height
    h =
      10

let -- | The default graph width
    w =
      24

let -- | The size of the axis label
    -- this is important to keep the timestamps aligned accross all dashboards
    axisWidth =
      96

let Prelude = ../../../../../Infra/Prelude.dhall

let Separator =
      { collapsed : Bool
      , gridPos : { h : Natural, w : Natural, x : Natural, y : Natural }
      , id : Natural
      , panels : List <>
      , title : Text
      , type : Text
      }

let OverrideDef =
      { matcher : { id : Text, options : Text }
      , properties :
          List { id : Text, value : { fixedColor : Text, mode : Text } }
      }

let ColorDef = { name : Text, color : Text }

let mkColorsOverrides =
      \(color : ColorDef) ->
        { matcher = { id = "byName", options = color.name }
        , properties =
          [ { id = "color"
            , value = { fixedColor = color.color, mode = "fixed" }
            }
          ]
        }

let Lucene =
      { datasource : { type : Text, uid : Text }
      , fieldConfig :
          { defaults :
              { color : { mode : Text }
              , custom :
                  { axisCenteredZero : Bool
                  , axisColorMode : Text
                  , axisLabel : Text
                  , axisPlacement : Text
                  , axisWidth : Natural
                  , barAlignment : Natural
                  , drawStyle : Text
                  , fillOpacity : Natural
                  , gradientMode : Text
                  , hideFrom : { legend : Bool, tooltip : Bool, viz : Bool }
                  , lineInterpolation : Text
                  , lineWidth : Natural
                  , pointSize : Natural
                  , scaleDistribution : { type : Text }
                  , showPoints : Text
                  , spanNulls : Bool
                  , stacking : { group : Text, mode : Text }
                  , thresholdsStyle : { mode : Text }
                  }
              , thresholds :
                  { mode : Text
                  , steps : List { color : Text, value : Optional Natural }
                  }
              , unit : Text
              }
          , overrides : List OverrideDef
          }
      , gridPos : { h : Natural, w : Natural, x : Natural, y : Natural }
      , hideTimeOverride : Bool
      , id : Natural
      , options :
          { legend :
              { calcs : List <>
              , displayMode : Text
              , placement : Text
              , showLegend : Bool
              }
          , tooltip : { mode : Text, sort : Text }
          }
      , targets :
          List
            { bucketAggs :
                List
                  { field : Text
                  , id : Text
                  , settings :
                      { interval : Optional Text
                      , min_doc_count : Text
                      , order : Optional Text
                      , orderBy : Optional Text
                      , size : Optional Text
                      }
                  , type : Text
                  }
            , datasource : { type : Text, uid : Text }
            , format : Text
            , hide : Bool
            , metrics : List { id : Text, type : Text }
            , query : Text
            , queryType : Text
            , refId : Text
            , timeField : Text
            }
      , title : Text
      , transformations : List <>
      , type : Text
      }

let PrometheusTarget =
      { datasource : { type : Text, uid : Text }
      , editorMode : Text
      , exemplar : Bool
      , expr : Text
      , format : Text
      , hide : Bool
      , instant : Bool
      , legendFormat : Text
      , range : Bool
      , refId : Text
      , interval : Text
      }

let mkPrometheusTarget =
      \(base : Natural) ->
      \(legendFormat : Text) ->
      \(expr : { index : Natural, value : Text }) ->
        { datasource = { type = "prometheus", uid = "P1809F7CD0C75ACF3" }
        , editorMode = "code"
        , exemplar = False
        , expr = expr.value
        , format = "time_series"
        , hide = False
        , instant = False
        , legendFormat
        , interval = "10m"
        , range = True
        , refId = "Q${Natural/show (base + expr.index)}"
        }

let Prometheus =
      { datasource : { type : Text, uid : Text }
      , fieldConfig :
          { defaults :
              { color : { mode : Text }
              , custom :
                  { axisCenteredZero : Bool
                  , axisColorMode : Text
                  , axisLabel : Text
                  , axisPlacement : Text
                  , axisWidth : Natural
                  , barAlignment : Natural
                  , drawStyle : Text
                  , fillOpacity : Natural
                  , gradientMode : Text
                  , hideFrom : { legend : Bool, tooltip : Bool, viz : Bool }
                  , lineInterpolation : Text
                  , lineWidth : Natural
                  , pointSize : Natural
                  , scaleDistribution : { type : Text }
                  , showPoints : Text
                  , spanNulls : Bool
                  , stacking : { group : Text, mode : Text }
                  , thresholdsStyle : { mode : Text }
                  }
              , min : Optional Natural
              , thresholds :
                  { mode : Text
                  , steps : List { color : Text, value : Optional <> }
                  }
              , unit : Text
              }
          }
      , gridPos : { h : Natural, w : Natural, x : Natural, y : Natural }
      , id : Natural
      , options :
          { legend :
              { displayMode : Text
              , placement : Text
              , showLegend : Bool
              , width : Natural
              }
          , tooltip : { mode : Text, sort : Text }
          }
      , targets : List PrometheusTarget
      , title : Text
      , type : Text
      }

let PType = < Sep : Separator | Lucene : Lucene | Prometheus : Prometheus >

let mkDashboard =
      \(title : Text) ->
      \(panels : List PType) ->
        { panels
        , title
        , editable = False
        , refresh = False
        , time = { from = "now-2d", to = "now" }
        , timepicker =
          { collapse = False
          , enable = True
          , notice = False
          , now = True
          , refresh_intervals =
            [ "5s", "10s", "30s", "1m", "5m", "15m", "30m", "1h", "2h", "1d" ]
          , status = "Stable"
          , time_options = [ "5m", "1h", "6h", "12h", "24h", "2d", "7d", "30d" ]
          , type = "timepicker"
          }
        , timezone = "utc"
        }

let mkSep =
      \(title : Text) ->
        PType.Sep
          { collapsed = False
          , gridPos = { h = 0, w, x = 0, y = 0 }
          , id = 1
          , panels = [] : List <>
          , title
          , type = "row"
          }

let mkLucene =
      \(title : Text) ->
      \(field : Text) ->
      \(colors : List ColorDef) ->
        PType.Lucene
          { datasource =
            { type = "grafana-opensearch-datasource"
            , uid = "P40D65588D5AED374"
            }
          , fieldConfig =
            { defaults =
              { color.mode = "palette-classic"
              , custom =
                { axisCenteredZero = False
                , axisColorMode = "text"
                , axisLabel = ""
                , axisPlacement = "auto"
                , axisWidth
                , barAlignment = 0
                , drawStyle = "bars"
                , fillOpacity = 100
                , gradientMode = "none"
                , hideFrom = { legend = False, tooltip = False, viz = False }
                , lineInterpolation = "linear"
                , lineWidth = 2
                , pointSize = 5
                , scaleDistribution.type = "linear"
                , showPoints = "never"
                , spanNulls = False
                , stacking = { group = "A", mode = "none" }
                , thresholdsStyle.mode = "off"
                }
              , thresholds =
                { mode = "absolute"
                , steps =
                  [ { color = "green", value = None Natural }
                  , { color = "red", value = Some 80 }
                  ]
                }
              , unit = "short"
              }
            , overrides =
                Prelude.List.map ColorDef OverrideDef mkColorsOverrides colors
            }
          , gridPos = { h, w, x = 0, y = 0 }
          , hideTimeOverride = False
          , id = 0
          , options =
            { legend =
              { calcs = [] : List <>
              , displayMode = "list"
              , placement = "bottom"
              , showLegend = True
              }
            , tooltip = { mode = "multi", sort = "none" }
            }
          , targets =
            [ { bucketAggs =
                [ { field
                  , id = "3"
                  , settings =
                    { interval = None Text
                    , min_doc_count = "1"
                    , order = Some "desc"
                    , orderBy = Some "_term"
                    , size = Some "10"
                    }
                  , type = "terms"
                  }
                , { field = "@timestamp"
                  , id = "2"
                  , settings =
                    { interval = Some "30m"
                    , min_doc_count = "1"
                    , order = None Text
                    , orderBy = None Text
                    , size = None Text
                    }
                  , type = "date_histogram"
                  }
                ]
              , datasource =
                { type = "grafana-opensearch-datasource"
                , uid = "P40D65588D5AED374"
                }
              , format = "table"
              , hide = False
              , metrics = [ { id = "1", type = "count" } ]
              , query = ""
              , queryType = "lucene"
              , refId = "A"
              , timeField = "@timestamp"
              }
            ]
          , title
          , transformations = [] : List <>
          , type = "timeseries"
          }

let mkPrometheusHelper =
      \(min : Optional Natural) ->
      \(legend : Text) ->
      \(err_exprs : List Text) ->
      \(title : Text) ->
      \(ok_exprs : List Text) ->
      \(unit : Text) ->
        PType.Prometheus
          { datasource = { type = "prometheus", uid = "P1809F7CD0C75ACF3" }
          , fieldConfig.defaults
            =
            { color.mode = "palette-classic"
            , custom =
              { axisCenteredZero = False
              , axisColorMode = "text"
              , axisLabel = ""
              , axisPlacement = "left"
              , axisWidth
              , barAlignment = 0
              , drawStyle = "line"
              , fillOpacity = 0
              , gradientMode = "none"
              , hideFrom = { legend = False, tooltip = False, viz = False }
              , lineInterpolation = "smooth"
              , lineWidth = 1
              , pointSize = 5
              , scaleDistribution.type = "linear"
              , showPoints = "never"
              , spanNulls = False
              , stacking = { group = "A", mode = "none" }
              , thresholdsStyle.mode = "off"
              }
            , min
            , thresholds =
              { mode = "absolute"
              , steps = [ { color = "green", value = None <> } ]
              }
            , unit
            }
          , gridPos = { h, w, x = 0, y = 0 }
          , id = 1
          , options =
            { legend =
              { displayMode = "list"
              , placement = "bottom"
              , showLegend = True
              , width = 250
              }
            , tooltip = { mode = "multi", sort = "desc" }
            }
          , targets =
                Prelude.List.map
                  { index : Natural, value : Text }
                  PrometheusTarget
                  (mkPrometheusTarget 0 legend)
                  (Prelude.List.indexed Text ok_exprs)
              # Prelude.List.map
                  { index : Natural, value : Text }
                  PrometheusTarget
                  ( mkPrometheusTarget
                      (List/length Text ok_exprs)
                      "${legend} error"
                  )
                  (Prelude.List.indexed Text err_exprs)
          , title
          , type = "timeseries"
          }

in  { mkSep
    , mkLucene
    , mkPrometheus =
        \(title : Text) ->
        \(expr : Text) ->
          mkPrometheusHelper
            (Some 0)
            "{{instance}}"
            ([] : List Text)
            title
            [ expr ]
    , mkPrometheusAppErr =
        \(title : Text) ->
        \(exprs : List Text) ->
        \(err_exprs : List Text) ->
          mkPrometheusHelper (Some 0) "{{job}}" err_exprs title exprs
    , mkPrometheusApp =
        \(title : Text) ->
        \(exprs : List Text) ->
          mkPrometheusHelper (Some 0) "{{job}}" ([] : List Text) title exprs
    , mkPrometheusMulti =
        mkPrometheusHelper (Some 0) "{{instance}}" ([] : List Text)
    , mkPrometheusZeroCentered =
        mkPrometheusHelper (None Natural) "{{instance}}" ([] : List Text)
    , mkDashboard
    , mkHosts = Prelude.Text.concatSep "|"
    }
