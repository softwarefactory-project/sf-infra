{ cidr : Optional Text
, dns_nameservers : List Text
, gateway_ip : Optional Text
, host_routes : Optional (List { destination : Text, nexthop : Text })
, name : Text
, network_name : Text
, ip_version : Optional ./IPVersion.dhall
, ipv6_address_mode : Optional ./IPV6AddressMode.dhall
}
