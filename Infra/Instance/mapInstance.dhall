--| Update a list of instance
let Instance = { Type = ./Type.dhall }

let mapInstance
    : forall (f : Instance.Type -> Instance.Type) ->
      List Instance.Type ->
        List Instance.Type
    = ./map.dhall Instance.Type

in  mapInstance
