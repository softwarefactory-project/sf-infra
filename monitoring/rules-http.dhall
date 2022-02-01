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
          ]
        }
      ]
    }
