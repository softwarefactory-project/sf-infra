{- The main record for an instance managed by sf-infra.

The attributes are used to manage the following files:

- vars/{tenant}.yaml
- ansible/hosts.yaml

Then the list can be used to easily iterate over all the
instances.
-}
{ Type =
    { name : Text
    , connection : (./Connection.dhall).Type
    , server : (./Server.dhall).Type
    , groups : List ../types/Group.dhall
    , volumes : List (./Volume.dhall).Type
    , skip_os_server_task : Bool
    , urls : List Text
    }
, default =
    { groups = [] : List ../types/Group.dhall
    , volumes = [] : List (./Volume.dhall).Type
    , skip_os_server_task = False
    , urls = [] : List Text
    }
}
