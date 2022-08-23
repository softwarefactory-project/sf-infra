let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "system-package-update-sf.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "SystemOutdatedPackagesSF"
            , for = Some "7d"
            , expr = Some
                ''
                system_package_update{instance!~"(centos.+|dlrn.+|rpm.+|trunk.+|www).rdoproject.org.+"} > 1
                ''
            , labels = Some
              { severity = "info"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary = "There are some packages to update in SF infra!"
              }
            }
          , Prometheus.CriticalRule::{
            , alert = Some "SystemOutdatedPackagesSF"
            , for = Some "14d"
            , expr = Some
                ''
                system_package_update{instance!~"(centos.+|dlrn.+|rpm.+|trunk.+|www).rdoproject.org.+"} > 100
                ''
            , labels = Some
              { severity = "warning"
              , lasttime = "{{ \$value | humanizeTimestamp }}"
              }
            , annotations = Some
              { description = None Text
              , summary = "There are many packages to update in SF infra!"
              }
            }
          ]
        }
      ]
    }
