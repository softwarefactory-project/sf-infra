--| Create a rule using host first.
--  This is useful when composed with integerMap to add tcp-rules for a single host.
let Rule =
      { Type = ./Type.dhall
      , default = ./default.dhall
      , integerMap = ./integerMap.dhall
      }

let createTcpHost
    : forall (ip : Text) -> forall (port : Integer) -> Rule.Type
    = \(ip : Text) -> \(port : Integer) -> ./create.dhall "tcp" port ip

let example0 =
        assert
      :     Rule.integerMap (createTcpHost "my-host") [ +9000, +9090 ]
        ===  [ Rule::{
               , port = +9000
               , protocol = Some "tcp"
               , remote_ip_prefix = Some "my-host"
               }
             , Rule::{
               , port = +9090
               , protocol = Some "tcp"
               , remote_ip_prefix = Some "my-host"
               }
             ]

in  createTcpHost
