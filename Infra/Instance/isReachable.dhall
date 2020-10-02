--| Check if an instance is reachable
let Instance = { Type = ./Type.dhall }

let isReachable
    : Instance.Type -> Bool
    = \(instance : Instance.Type) ->
        ../Connection/hasProxy.dhall instance.connection == False

in  isReachable
