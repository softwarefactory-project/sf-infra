{-|
# Server

A package to define OpenStack `Server`.
-}
{ Ip = ./Ip.dhall
, Type = ./Type.dhall
, FloatingIp = ./FloatingIp.dhall
, State = ./State.dhall
, addSecurityGroups = ./addSecurityGroups.dhall
, default = ./default.dhall
, getName = ./getName.dhall
, map = ./map.dhall
, isPresent =
    \(server : ./Type.dhall) ->
      merge { present = True, absent = False } server.state
}
