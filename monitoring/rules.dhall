{-
Some doc:
  Check alert status: curl -g 'http://localhost:9090/prometheus/api/v1/alerts' | python -mjson.tool
  Get metrics names: curl -g 'http://localhost:9090/prometheus/api/v1/label/__name__/values' | tr ',' '\n'
  Test expr: curl -g 'http://localhost:9090/prometheus/api/v1/query' --data-urlencode 'query=predict_linear(node_filesystem_avail_bytes{job="node",mountpoint="/",instance="managesf.softwarefactory-project.io:9100"}[1d], 365 * 24 * 3600)'
-}
let Prometheus = ./binding.dhall

let gigabyte = "1024^3"

let megabyte = "1024^2"

in  \(job-name : Text) ->
      Prometheus.RulesConfig::{
      , groups = Some
        [ Prometheus.Group::{
          , name = Some "${job-name}.rules"
          , rules = Some
            [ Prometheus.CriticalRule::{
              , alert = Some "InstanceDown"
              , expr = Some "up{job='${job-name}'} == 0"
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

                  let avail = "node_filesystem_avail_bytes{job='${job-name}'}"

                  let total = "node_filesystem_size_bytes{job='${job-name}'}"

                  let usage = "${avail} * 100 / ${total}"

                  let prediction =
                        "predict_linear(node_filesystem_avail_bytes{job='${job-name}'}[1d], 3 * 24 * 3600)"

                  let less_than_100G =
                        "node_filesystem_size_bytes{job='${job-name}'} <= 1e+11"

                  in  Some
                        "(${usage} < 50) and (${prediction} < 0) and (${less_than_100G})"
              , for = Some "12h"
              , annotations = Some Prometheus.Annotations::{
                , summary =
                    "Out of disk space (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Disk is filling up ({{ $value | humanize }}% left)
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
                        "predict_linear(node_memory_MemAvailable_bytes{job='${job-name}'}[1d], 3 * 24 * 3600)"

                  in  Some
                        "(${usage} < 50) and (${prediction} < 0) AND {instance!~'ze.*|zm.*', job='node'}"
              , for = Some "12h"
              , annotations = Some Prometheus.Annotations::{
                , summary = "Out of memory (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Node memory is filling up ({{ $value | humanize }}% left)
                      LABELS: {{ $labels }}
                    ''
                }
              }
            , Prometheus.CriticalRule::{
              , alert = Some "InstanceOutOfMemory"
              , expr =
                  let avail =
                        "node_memory_MemAvailable_bytes{job='${job-name}'}"

                  let free = "node_memory_SwapFree_bytes{job='${job-name}'}"

                  let total = "node_memory_MemTotal_bytes{job='${job-name}'}"

                  in  Some "(${avail} + ${free}) / ${total} * 100 < 10"
              , for = Some "30m"
              , annotations = Some Prometheus.Annotations::{
                , summary = "Out of memory (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Node only has {{ $value | humanize }}% of free mem available.
                    ''
                }
              }
            , Prometheus.AlertingRule::{
              , alert = Some "InstanceOutOfSwap"
              , expr =
                  let total = "node_memory_SwapTotal_bytes{job='${job-name}'}"

                  let free = "node_memory_SwapFree_bytes{job='${job-name}'}"

                  in  Some
                        "(${free} < ${total} * 0.5) and node_memory_SwapTotal_bytes{instance!='quay.rdoproject.org:9100'}"
              , for = Some "30m"
              , annotations = Some Prometheus.Annotations::{
                , summary = "Out of swap (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Node only has {{ $value | humanize1024 }} of free swap available.
                    ''
                }
              }
            , Prometheus.CriticalRule::{
              , alert = Some "InstanceOutOfDisk"
              , expr =
                  let percentage_node_filesystem_avail_bytes =
                        "node_filesystem_avail_bytes{fstype!='tmpfs',fstype!='rootfs',job='${job-name}'} * 100 / node_filesystem_size_bytes{fstype!='tmpfs',fstype!='rootfs',job='${job-name}'}"

                  let filesystem_avail_bytes =
                        "node_filesystem_avail_bytes{fstype!='tmpfs',fstype!='rootfs'}"

                  in  Some
                        "(${percentage_node_filesystem_avail_bytes} < 10) and (${filesystem_avail_bytes} < 20 * ${gigabyte})"
              , for = Some "30m"
              , annotations = Some Prometheus.Annotations::{
                , summary = "Out of disk (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    Node only has {{ $value | humanize }}% of free disk available.
                    ''
                }
              }
            , Prometheus.AlertingRule::{
              , alert = Some "OOMKillerDetected"
              , expr = Some "increase(node_vmstat_oom_kill[10m]) > 0"
              , for = Some "5m"
              , annotations = Some Prometheus.Annotations::{
                , summary =
                    "OOM Killer detected on (instance {{ \$labels.instance }})"
                , description = Some
                    ''
                    OOM kill detected
                      VALUE = {{ $value }}
                      LABELS: {{ $labels }}''
                }
              }
            ]
          }
        ]
      }
