let Instance = ../Infra/Instance/package.dhall

let instances = ./instances.dhall

in  { instances
    , servers =
        Instance.getServers (Instance.filter Instance.isCreated instances)
    }
