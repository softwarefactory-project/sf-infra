-- | To generate a new panel, copy the panel JSON from the UI.
-- Use `json-to-dhall | dhall type` to generate the type and add it to the 'PType' union.
-- Then use `json-to-dhall` to create a mkHelper, moving the common values as function parameters.
--
\(datasourceId : Text) ->
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

  let ExprLeg -- A prometheus query and its legend
              =
        { query : Text, legend : Text }

  let mkPrometheusTarget =
        \(base : Natural) ->
        \(expr : { index : Natural, value : ExprLeg }) ->
          { datasource = { type = "prometheus", uid = datasourceId }
          , editorMode = "code"
          , exemplar = False
          , expr = expr.value.query
          , format = "time_series"
          , hide = False
          , instant = False
          , legendFormat = expr.value.legend
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

  let Table =
        { datasource : { type : Text, uid : Text }
        , description : Text
        , fieldConfig :
            { defaults :
                { color : { mode : Text }
                , custom :
                    { align : Text
                    , cellOptions : { type : Text }
                    , inspect : Bool
                    }
                , mappings : List <>
                , thresholds :
                    { mode : Text
                    , steps : List { color : Text, value : Optional Natural }
                    }
                }
            , overrides :
                List
                  { matcher : { id : Text, options : Text }
                  , properties : List { id : Text, value : Text }
                  }
            }
        , gridPos : { h : Natural, w : Natural, x : Natural, y : Natural }
        , id : Natural
        , links : List { title : Text, url : Text }
        , options :
            { cellHeight : Text
            , footer :
                { countRows : Bool
                , enablePagination : Bool
                , fields : List Text
                , reducer : List Text
                , show : Bool
                }
            , showHeader : Bool
            , sortBy : List { desc : Bool, displayName : Text }
            }
        , pluginVersion : Text
        , targets :
            List
              { datasource : { type : Text, uid : Text }
              , disableTextWrap : Bool
              , editorMode : Text
              , exemplar : Bool
              , expr : Text
              , fullMetaSearch : Bool
              , hide : Bool
              , includeNullMetadata : Bool
              , instant : Bool
              , legendFormat : Text
              , range : Bool
              , refId : Text
              , useBackend : Bool
              }
        , title : Text
        , transformations :
            List
              { filter : Optional { id : Text, options : Text }
              , id : Text
              , options :
                  { byField : Optional Text
                  , conversions :
                      Optional
                        ( List
                            { destinationType : Text
                            , enumConfig : Optional { text : List <> }
                            , targetField : Text
                            }
                        )
                  , excludeByName :
                      Optional
                        { `instance 1` : Bool
                        , `instance 2` : Bool
                        , `softwarefactory 1` : Bool
                        , `softwarefactory 2` : Bool
                        }
                  , fields : Optional {}
                  , include : Optional { names : List Text }
                  , includeByName : Optional {}
                  , indexByName :
                      Optional
                        { `All unique values 1` : Natural
                        , `All unique values 2` : Natural
                        , diskimage : Natural
                        , ext : Natural
                        , `instance 1` : Natural
                        , `instance 2` : Natural
                        , `softwarefactory 1` : Natural
                        , `softwarefactory 2` : Natural
                        }
                  , keepTime : Optional Bool
                  , mode : Optional Text
                  , reducers : Optional (List Text)
                  , renameByName :
                      Optional
                        { `All unique values` : Text
                        , `All unique values 1` : Text
                        , `All unique values 2` : Text
                        , diskimage : Text
                        , `softwarefactory 1` : Text
                        }
                  , replace : Optional Bool
                  , source : Optional Text
                  }
              , topic : Optional Text
              }
        , type : Text
        }

  let PType =
        < Sep : Separator
        | Lucene : Lucene
        | Prometheus : Prometheus
        | TablePanel : Table
        >

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
            , time_options =
              [ "5m", "1h", "6h", "12h", "24h", "2d", "7d", "30d" ]
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
        \(datasourceId : Text) ->
        \(title : Text) ->
        \(field : Text) ->
        \(colors : List ColorDef) ->
          PType.Lucene
            { datasource =
              { type = "grafana-opensearch-datasource", uid = datasourceId }
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
                  { type = "grafana-opensearch-datasource", uid = datasourceId }
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
        \(err_exprs : List ExprLeg) ->
        \(title : Text) ->
        \(ok_exprs : List ExprLeg) ->
        \(unit : Text) ->
          PType.Prometheus
            { datasource = { type = "prometheus", uid = datasourceId }
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
                    { index : Natural, value : { query : Text, legend : Text } }
                    PrometheusTarget
                    (mkPrometheusTarget 0)
                    ( Prelude.List.indexed
                        { query : Text, legend : Text }
                        ok_exprs
                    )
                # Prelude.List.map
                    { index : Natural, value : { query : Text, legend : Text } }
                    PrometheusTarget
                    ( mkPrometheusTarget
                        (List/length { query : Text, legend : Text } ok_exprs)
                    )
                    ( Prelude.List.indexed
                        { query : Text, legend : Text }
                        err_exprs
                    )
            , title
            , type = "timeseries"
            }

  let mkPrometheus =
        \(title : Text) ->
        \(exprs : List ExprLeg) ->
          mkPrometheusHelper (Some 0) ([] : List ExprLeg) title exprs

  let mkPrometheusErr =
        \(title : Text) ->
        \(exprs : List ExprLeg) ->
        \(err_exprs : List ExprLeg) ->
          mkPrometheusHelper (Some 0) err_exprs title exprs

  let mkPrometheusMulti = mkPrometheusHelper (Some 0) ([] : List ExprLeg)

  let mkPrometheusZeroCentered =
        mkPrometheusHelper (None Natural) ([] : List ExprLeg)

  let mkTable =
        \(title : Text) ->
        \(description : Text) ->
        \(links : List { title : Text, url : Text }) ->
        \ ( overrides
          : List
              { matcher : { id : Text, options : Text }
              , properties : List { id : Text, value : Text }
              }
          ) ->
        \ ( targets
          : List
              { datasource : { type : Text, uid : Text }
              , disableTextWrap : Bool
              , editorMode : Text
              , exemplar : Bool
              , expr : Text
              , fullMetaSearch : Bool
              , hide : Bool
              , includeNullMetadata : Bool
              , instant : Bool
              , legendFormat : Text
              , range : Bool
              , refId : Text
              , useBackend : Bool
              }
          ) ->
        \ ( transformations
          : List
              { filter : Optional { id : Text, options : Text }
              , id : Text
              , options :
                  { byField : Optional Text
                  , conversions :
                      Optional
                        ( List
                            { destinationType : Text
                            , enumConfig : Optional { text : List <> }
                            , targetField : Text
                            }
                        )
                  , excludeByName :
                      Optional
                        { `instance 1` : Bool
                        , `instance 2` : Bool
                        , `softwarefactory 1` : Bool
                        , `softwarefactory 2` : Bool
                        }
                  , fields : Optional {}
                  , include : Optional { names : List Text }
                  , includeByName : Optional {}
                  , indexByName :
                      Optional
                        { `All unique values 1` : Natural
                        , `All unique values 2` : Natural
                        , diskimage : Natural
                        , ext : Natural
                        , `instance 1` : Natural
                        , `instance 2` : Natural
                        , `softwarefactory 1` : Natural
                        , `softwarefactory 2` : Natural
                        }
                  , keepTime : Optional Bool
                  , mode : Optional Text
                  , reducers : Optional (List Text)
                  , renameByName :
                      Optional
                        { `All unique values` : Text
                        , `All unique values 1` : Text
                        , `All unique values 2` : Text
                        , diskimage : Text
                        , `softwarefactory 1` : Text
                        }
                  , replace : Optional Bool
                  , source : Optional Text
                  }
              , topic : Optional Text
              }
          ) ->
        \(sortBy : List { desc : Bool, displayName : Text }) ->
          PType.TablePanel
            { datasource = { type = "prometheus", uid = datasourceId }
            , description
            , fieldConfig =
              { defaults =
                { color.mode = "thresholds"
                , custom =
                  { align = "auto", cellOptions.type = "auto", inspect = False }
                , mappings = [] : List <>
                , thresholds =
                  { mode = "absolute"
                  , steps =
                    [ { color = "green", value = None Natural }
                    , { color = "red", value = Some 80 }
                    ]
                  }
                }
              , overrides
              }
            , gridPos = { h, w, x = 0, y = 0 }
            , id = 3
            , links
            , options =
              { cellHeight = "sm"
              , footer =
                { countRows = False
                , enablePagination = False
                , fields = [ "All unique values" ]
                , reducer = [ "sum" ]
                , show = True
                }
              , showHeader = True
              , sortBy
              }
            , pluginVersion = "10.4.2"
            , targets
            , title
            , transformations
            , type = "table"
            }

  let nodeMetrics =
        \(hostList : List Text) ->
          let hosts = Prelude.Text.concatSep "|" hostList

          let -- | Memory usage
              -- - What: the amount of available memory
              -- - Why: watchout for out of memory errors
              mem =
                mkPrometheus
                  "Available Memory"
                  [ { query =
                        "avg by (instance) (node_memory_MemFree_bytes{instance=~\"${hosts}\"})"
                    , legend = "{{instance}}"
                    }
                  ]
                  "decbytes"

          let -- | Disk usage
              -- - What: The amount of available disk space clamped
              -- - Why: watchout for out of disk errors
              disk =
                let maxGB = 10

                let maxStr = Natural/show (maxGB * 1024 * 1024 * 1024)

                let mkQuery =
                      \(filter : Text) ->
                        { query =
                            "clamp_max(avg by(instance) (node_filesystem_avail_bytes{${filter}}), ${maxStr})"
                        , legend = "{{instance}}"
                        }

                in  mkPrometheusMulti
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
                mkPrometheusZeroCentered
                  "Network Load (recv are positive, sent are negative)"
                  [ { query =
                        "irate(node_network_receive_bytes_total{instance=~\"${hosts}\", device=\"eth0\"}[\$__rate_interval])*8"
                    , legend = "{{instance}}"
                    }
                  , { query =
                        "irate(node_network_transmit_bytes_total{instance=~\"${hosts}\", device=\"eth0\"}[\$__rate_interval])*(-8)"
                    , legend = "{{instance}}"
                    }
                  ]
                  "bps"

          let -- | CPU usage
              -- - What: the load average
              -- - Why: watchout for fork bomb
              cpu =
                mkPrometheus
                  "CPU Load"
                  [ { query =
                        "avg by (instance) (rate(node_cpu_seconds_total{mode=~\"system|user\",instance=~\"${hosts}\"}[1h]))"
                    , legend = "{{instance}}"
                    }
                  ]
                  "percentunit"

          in  [ mkSep "Systems", mem, disk, net, cpu ]

  in  { datasourceId
      , mkSep
      , mkLucene
      , mkPrometheus
      , mkPrometheusErr
      , mkPrometheusMulti
      , mkPrometheusZeroCentered
      , mkDashboard
      , nodeMetrics
      , mkTable
      }
