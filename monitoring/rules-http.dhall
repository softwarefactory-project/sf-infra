let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "blackbox.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "WebDown"
            , expr = Some "probe_success{job='blackbox'} == 0"
            , for = Some "10m"
            }
          , Prometheus.CriticalRule::{
            , alert = Some "SSLCertExpiringSoon"
            , expr = Some
                "probe_ssl_earliest_cert_expiry{job='blackbox'} - time() < 86400 * 6"
            , for = Some "1d"
            }
          , Prometheus.CriticalRule::{
            , alert = Some "HighHTTP503Errors"
            , expr = Some "rate(http_requests_total{status='503'}[10m]) > 0"
            , for = Some "10m"
            , annotations = Some Prometheus.Annotations::{
              , summary = "Unresponsive service (instance {{ $labels.instance }})"
              , description = Some ''
                  The service {{ $labels.instance }} has been consistently returning HTTP 503 errors in the last 10 minutes.
                  ''
              }
            }
          ]
        }
      ]
    }
