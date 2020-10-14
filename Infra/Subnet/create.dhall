--| A Subnet constructor for /24 network
let Subnet = { Type = ./Type.dhall, default = ./default.dhall }

let create
    : forall (name : Text) ->
      forall (network_prefix : Text) ->
      forall (dns_nameservers : List Text) ->
        Subnet.Type
    = ./createWithMask.dhall "24"

let example0 =
        assert
      :     create "mynet" "10.0.0" [ "10.1.1.1" ]
        ===  Subnet::{
             , name = "mynet-subnet"
             , cidr = "10.0.0.0/24"
             , gateway_ip = Some "10.0.0.1"
             , dns_nameservers = [ "10.1.1.1" ]
             , network_name = "mynet-network"
             }

in  create
