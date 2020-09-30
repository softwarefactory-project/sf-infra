let Rule = { Type = ./Type.dhall, default = ./default.dhall }

in  \(proto : Text) ->
    \(port : Integer) ->
    \(ip : Text) ->
      Rule::{ port, protocol = Some proto, remote_ip_prefix = Some ip }
