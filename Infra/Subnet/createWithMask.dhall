--| A Subnet constructor
let Subnet = { Type = ./Type.dhall, default = ./default.dhall }

let createWithMask
    : forall (mask : Text) ->
      forall (name : Text) ->
      forall (network_prefix : Text) ->
      forall (dns_nameservers : List Text) ->
        Subnet.Type
    = \(mask : Text) ->
      \(name : Text) ->
      \(network_prefix : Text) ->
      \(dns_nameservers : List Text) ->
        Subnet::{
        , name = name ++ "-subnet"
        , cidr = network_prefix ++ ".0/" ++ mask
        , gateway_ip = Some (network_prefix ++ ".1")
        , dns_nameservers
        , network_name = name ++ "-network"
        }

let example0 =
        assert
      :     createWithMask "16" "mynet" "10.0.0" [ "10.1.1.1" ]
        ===  Subnet::{
             , name = "mynet-subnet"
             , cidr = "10.0.0.0/16"
             , gateway_ip = Some "10.0.0.1"
             , dns_nameservers = [ "10.1.1.1" ]
             , network_name = "mynet-network"
             }

in  createWithMask
