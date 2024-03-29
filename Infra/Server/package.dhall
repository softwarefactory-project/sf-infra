{-|
# Server

A package to define OpenStack `Server`.
-}
{ Type = ./Type.dhall
, State = ./State.dhall
, addSecurityGroups = ./addSecurityGroups.dhall
, default = ./default.dhall
, getName = ./getName.dhall
, map = ./map.dhall
, show = (./show.dhall).show
, maybeShow = (./show.dhall).maybeShow
, isPresent =
    \(server : ./Type.dhall) ->
      merge { present = True, absent = False } server.state
}
