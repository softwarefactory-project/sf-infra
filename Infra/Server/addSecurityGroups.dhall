\(security-groups : List Text) ->
\(server : ./Type.dhall) ->
  server // { security_groups = security-groups # server.security_groups }
