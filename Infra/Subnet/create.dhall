--| A Subnet constructor for /24 network
let Subnet = { Type = ./Type.dhall, default = ./default.dhall }

let create
    : forall (network_name : Text) ->
      forall (subnet_name : Text) ->
      forall (network_prefix : Text) ->
      forall (dns_nameservers : List Text) ->
        Subnet.Type
    = ./createWithMask.dhall "24"

let example0 =
        assert
      :     create "my-network" "my-subnet" "10.0.0" [ "10.1.1.1" ]
        ===  Subnet::{
             , name = "my-subnet"
             , cidr = "10.0.0.0/24"
             , gateway_ip = "10.0.0.1"
             , dns_nameservers = [ "10.1.1.1" ]
             , network_name = "my-network"
             }

in  create
