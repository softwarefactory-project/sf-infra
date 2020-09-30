--| The recommanded nomenclature for network
let create =
      \(network : Text) ->
      \(name : Text) ->
          { name = name ++ "-network"
          , external_network = network
          , port_security_enabled = False
          }
        : ./Type.dhall

in  create
