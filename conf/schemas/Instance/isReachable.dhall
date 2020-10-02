--| Check if an instance is reachable
\(instance : ./Type.dhall) ->
  ../Connection/hasProxy.dhall instance.connection == False
