--| This functions add security groups and set the fqdn of a list of instances
let Instance = ../Instance/package.dhall

let Server = ../Server/package.dhall

in  \(security-groups : List Text) ->
    \(fqdn : Text) ->
    \(instances : List Instance.Type) ->
      Instance.mapInstance
        (Instance.updateServer (Server.addSecurityGroups security-groups))
        (Instance.mapInstance (Instance.setFqdn fqdn) instances)
