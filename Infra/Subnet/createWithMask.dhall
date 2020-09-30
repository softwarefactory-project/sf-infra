\(mask : Text) ->
\(name : Text) ->
\(network_prefix : Text) ->
\(dns_nameservers : List Text) ->
    { name = name ++ "-subnet"
    , cidr = network_prefix ++ ".0/" ++ mask
    , gateway_ip = network_prefix ++ ".1"
    , dns_nameservers
    , network_name = name ++ "-network"
    }
  : ./Type.dhall
