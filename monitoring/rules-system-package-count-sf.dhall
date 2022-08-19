let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "system-package-update-sf.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "SystemOutdatedPackagesSF"
            , expr = Some
                ''
                system_package_update{instance!~"(centos.+|dlrn.+|rpm.+|trunk.+).rdoproject.org.+"} > 1
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
            , expr = Some
                ''
                system_package_update{instance!~"(centos.+|dlrn.+|rpm.+|trunk.+).rdoproject.org.+"} > 100
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
