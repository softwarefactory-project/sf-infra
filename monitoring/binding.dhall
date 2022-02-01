{- This file defines how to import the dhall-prometheus binding
   The DHALL_PROMETHEUS environment variables is used in the CI to use a cached version.
-}
let Prometheus =
        env:DHALL_PROMETHEUS sha256:0b895a16c4826c6b10ff44cbc647045c640fcc6742d1cbf4f5be4a4f92c168e7
      ? https://raw.githubusercontent.com/TristanCacqueray/dhall-prometheus/d76c58272c269bd082de558a3ecf10e1e80f1727/package.dhall sha256:0b895a16c4826c6b10ff44cbc647045c640fcc6742d1cbf4f5be4a4f92c168e7

let -- | An extra schema for critical rules that sends email
    extra =
      { CriticalRule =
        { Type = Prometheus.AlertingRule.Type
        , default =
                Prometheus.AlertingRule.default
            //  { labels = Some
                  { severity = "critical"
                  , lasttime = "{{ \$value | humanizeTimestamp }}"
                  }
                }
        }
      }

in  Prometheus // extra
