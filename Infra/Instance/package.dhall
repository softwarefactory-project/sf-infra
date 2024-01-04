{-|
# Instance

A package to abstract OpenStack and provide a higher level representation of
infrastructure component.
-}
{ Type = ./Type.dhall
, default = ./default.dhall
, filter = ./filter.dhall
, keepPresent = ./keepPresent.dhall
, generate = ./generate.dhall
, getName = ./getName.dhall
, getNodeExporter = ./getNodeExporter.dhall
, getBackup = ./getBackup.dhall
, getBackups = ./getBackups.dhall
, getServer = ./getServer.dhall
, getServers = ./getServers.dhall
, isCreated = ./isCreated.dhall
, isReachable = ./isReachable.dhall
, map = ./map.dhall
, mapInstance = ./mapInstance.dhall
, setFqdn = ./setFqdn.dhall
, setName = ./setName.dhall
, textMap = ./textMap.dhall
, updateServer = ./updateServer.dhall
}
