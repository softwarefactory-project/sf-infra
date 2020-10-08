{-|
# Instance

A package to abstract OpenStack and provide a higher level representation of
infrastructure component.
-}
{ External = ./External.dhall
, Type = ./Type.dhall
, default = ./default.dhall
, filter = ./filter.dhall
, generate = ./generate.dhall
, getName = ./getName.dhall
, getServer = ./getServer.dhall
, getServers = ./getServers.dhall
, isCreated = ./isCreated.dhall
, isReachable = ./isReachable.dhall
, map = ./map.dhall
, mapInstance = ./mapInstance.dhall
, setFqdn = ./setFqdn.dhall
, textMap = ./textMap.dhall
, updateServer = ./updateServer.dhall
}
