--| Check if a connection use a proxy
let Connection = { Type = ./Type.dhall, default = ./default.dhall }

let hasProxy
    : Connection.Type -> Bool
    = \(conn : ./Type.dhall) ->
        merge
          { None = False, Some = \(proxy : Text) -> True }
          conn.proxy_command

let example0 = assert : hasProxy Connection::{=} === False

in  hasProxy
