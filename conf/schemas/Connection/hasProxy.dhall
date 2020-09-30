--| Check if a connection use a proxy
\(conn : ./Type.dhall) ->
  merge { None = False, Some = \(proxy : Text) -> True } conn.proxy_command
