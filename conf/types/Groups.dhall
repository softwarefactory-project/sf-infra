{- This file is generated by python -}
let Group = ./Group.dhall

in  { show =
            \(group : Group)
        ->  merge
              { afs-mirror = "afs-mirror"
              , ara = "ara"
              , backup = "backup"
              , install-server-sf = "install-server-sf"
              , sf = "sf"
              , logreduce-mqtt = "logreduce-mqtt"
              , dlrn = "dlrn"
              , registry = "registry"
              }
              group
    , groups =
      [ { value = Group.afs-mirror
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = True
                  , ara = False
                  , backup = False
                  , install-server-sf = False
                  , sf = False
                  , logreduce-mqtt = False
                  , dlrn = False
                  , registry = False
                  }
                  group
        }
      , { value = Group.ara
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = False
                  , ara = True
                  , backup = False
                  , install-server-sf = False
                  , sf = False
                  , logreduce-mqtt = False
                  , dlrn = False
                  , registry = False
                  }
                  group
        }
      , { value = Group.backup
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = False
                  , ara = False
                  , backup = True
                  , install-server-sf = False
                  , sf = False
                  , logreduce-mqtt = False
                  , dlrn = False
                  , registry = False
                  }
                  group
        }
      , { value = Group.install-server-sf
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = False
                  , ara = False
                  , backup = False
                  , install-server-sf = True
                  , sf = False
                  , logreduce-mqtt = False
                  , dlrn = False
                  , registry = False
                  }
                  group
        }
      , { value = Group.sf
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = False
                  , ara = False
                  , backup = False
                  , install-server-sf = False
                  , sf = True
                  , logreduce-mqtt = False
                  , dlrn = False
                  , registry = False
                  }
                  group
        }
      , { value = Group.logreduce-mqtt
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = False
                  , ara = False
                  , backup = False
                  , install-server-sf = False
                  , sf = False
                  , logreduce-mqtt = True
                  , dlrn = False
                  , registry = False
                  }
                  group
        }
      , { value = Group.dlrn
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = False
                  , ara = False
                  , backup = False
                  , install-server-sf = False
                  , sf = False
                  , logreduce-mqtt = False
                  , dlrn = True
                  , registry = False
                  }
                  group
        }
      , { value = Group.registry
        , test =
                \(group : Group)
            ->  merge
                  { afs-mirror = False
                  , ara = False
                  , backup = False
                  , install-server-sf = False
                  , sf = False
                  , logreduce-mqtt = False
                  , dlrn = False
                  , registry = True
                  }
                  group
        }
      ]
    , Type = { value : Group, test : forall (group : Group) -> Bool }
    }
