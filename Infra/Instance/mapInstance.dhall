--| Update a list of instance
let mapInstance
    : forall (f : ./Type.dhall -> ./Type.dhall) ->
      List ./Type.dhall ->
        List ./Type.dhall
    = ./map.dhall ./Type.dhall

in  mapInstance
