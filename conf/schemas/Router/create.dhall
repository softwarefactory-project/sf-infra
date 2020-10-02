\(network : Text) ->
\(name : Text) ->
\(network_prefix : Text) ->
    { name = name ++ "-router"
    , network
    , port_security_enabled = None Bool
    , interfaces =
      [ { net = name ++ "-network"
        , subnet = name ++ "-subnet"
        , portip = network_prefix ++ ".1"
        }
      ]
    }
  : ./Type.dhall
