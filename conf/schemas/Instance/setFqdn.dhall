\(fqdn : Text) ->
\(instance : ./Type.dhall) ->
  instance // { name = instance.name ++ "." ++ fqdn }
