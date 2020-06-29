{-
Some doc:
  Check alert status: curl -g 'http://localhost:9090/prometheus/api/v1/alerts' | python -mjson.tool
  Get metrics names: curl -g 'http://localhost:9090/prometheus/api/v1/label/__name__/values' | tr ',' '\n'
  Test expr: curl -g 'http://localhost:9090/prometheus/api/v1/query' --data-urlencode 'query=predict_linear(node_filesystem_avail_bytes{job="node",mountpoint="/",instance="managesf.softwarefactory-project.io:9100"}[1d], 365 * 24 * 3600)'
-}

let Prometheus = ./binding.dhall

in  \(job-name : Text) ->
      Prometheus.RulesConfig::{
      , groups = Some
        [ Prometheus.Group::{
          , name = Some "${job-name}.rules"
          , rules = Some
            [ Prometheus.AlertingRule::{
              , alert = Some "InstanceDown"
              , expr = Some "up{job=\"${job-name}\"} == 0"
              , for = Some "10m"
              }
            , Prometheus.AlertingRule::{
              , alert = Some "WillRunOutOfDiskInThreeDays"
              , expr =
                  let comment =
                        ''
                        When avail disk space is < 50% (to preven false alarm when server is warming up)
                        Over 1 day, if the slope becomes negative in 3 days, then emit an alarm
                        ''

                  let avail = "node_filesystem_avail_bytes{job=\"${job-name}\"}"

                  let total = "node_filesystem_size_bytes{job=\"${job-name}\"}"

                  let usage = "${avail} * 100 / ${total}"

                  let prediction =
                        "predict_linear(node_filesystem_avail_bytes{job=\"${job-name}\"}[1d], 3 * 24 * 3600)"

                  in  Some "(${usage} < 50) and (${prediction} < 0)"
              , for = Some "12h"
              , annotations = Some Prometheus.Annotations::{
                , summary =
                    "Out of disk space (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Disk is almost full (< 10% left)
                      VALUE = {{ $value }}
                      LABELS: {{ $labels }}''
                }
              }
            , Prometheus.AlertingRule::{
              , alert = Some "WillRunOutOfMemoryInThreeDays"
              , expr =
                  let avail = "node_memory_MemAvailable_bytes"

                  let total = "node_memory_MemTotal_bytes"

                  let usage = "${avail} * 100 / ${total}"

                  let prediction =
                        "predict_linear(node_memory_MemAvailable_bytes{job=\"${job-name}\"}[1d], 3 * 24 * 3600)"

                  in  Some "(${usage} < 50) and (${prediction} < 0)"
              , for = Some "12h"
              , annotations = Some Prometheus.Annotations::{
                , summary = "Out of memory (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Node memory is filling up (< 10% left)
                      VALUE = {{ $value }}
                      LABELS: {{ $labels }}
                    ''
                }
              }
            , Prometheus.AlertingRule::{
              , alert = Some "InstanceOutOfMemory"
              , expr = Some
                  "node_memory_MemAvailable_bytes < (1024 * 1024 * 10)"
              , for = Some "30m"
              , annotations = Some Prometheus.Annotations::{
                , summary = "Out of memory (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Node only has {{ $value }} bytes of free mem available.
                    ''
                }
              }
            ]
          }
        ]
      }
