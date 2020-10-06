--| Check if an instance has been created
let Instance = { Type = ./Type.dhall }

let isCreated
    : Instance.Type -> Bool
    = \(instance : Instance.Type) ->
        merge
          { None = False, Some = \(server : ../Server/Type.dhall) -> True }
          instance.server

in  isCreated
