let Prometheus = ./binding.dhall

in  Prometheus.RulesConfig::{
    , groups = Some
      [ Prometheus.Group::{
        , name = Some "mysqld.rules"
        , rules = Some
          [ Prometheus.CriticalRule::{
            , alert = Some "MariaDBDown"
            , expr = Some "mysql_up{job='mysqld'} == 0"
            , for = Some "10m"
            , annotations = Some
              { description = None Text
              , summary = "MariaDB is down on {{ \$labels.instance }}"
              }
            }
          , Prometheus.CriticalRule::{
            , alert = Some "MariaDBReplicationFailed"
            , expr = Some
                "(mysql_slave_status_slave_io_running{job='mysqld'} == 0) or (mysql_slave_status_slave_sql_running{job='mysqld'} == 0)"
            , for = Some "10m"
            , annotations = Some
              { description = None Text
              , summary =
                  "MariaDB replication is failing on {{ \$labels.instance }}"
              }
            }
          ]
        }
      ]
    }
