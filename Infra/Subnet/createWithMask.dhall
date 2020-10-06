--| A Subnet constructor
let Subnet = { Type = ./Type.dhall, default = ./default.dhall }

let createWithMask
    : forall (mask : Text) ->
      forall (network_name : Text) ->
      forall (subnet_name : Text) ->
      forall (network_prefix : Text) ->
      forall (dns_nameservers : List Text) ->
        Subnet.Type
    = \(mask : Text) ->
      \(network_name : Text) ->
      \(subnet_name : Text) ->
      \(network_prefix : Text) ->
      \(dns_nameservers : List Text) ->
        { name = subnet_name
        , cidr = network_prefix ++ ".0/" ++ mask
        , gateway_ip = network_prefix ++ ".1"
        , dns_nameservers
        , network_name
        }

let example0 =
        assert
      :     createWithMask "16" "my-network" "my-subnet" "10.0.0" [ "10.1.1.1" ]
        ===  Subnet::{
             , name = "my-subnet"
             , cidr = "10.0.0.0/16"
             , gateway_ip = "10.0.0.1"
             , dns_nameservers = [ "10.1.1.1" ]
             , network_name = "my-network"
             }

in  createWithMask
