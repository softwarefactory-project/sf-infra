let Instance = ../Infra/Instance/package.dhall

let instances = ./instances.dhall

in  { instances
    , servers =
        Instance.getServers (Instance.filter Instance.isCreated instances)
    , common = ./common.dhall
    , infra-rdo = ./infra-rdo/package.dhall
    , infra-sf = ./infra-sf/package.dhall
    }
