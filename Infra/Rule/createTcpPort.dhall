--| Create a rule using host first.
--  This is useful when composed with textMap to add tcp-rules for a single port.
let Rule =
      { Type = ./Type.dhall
      , default = ./default.dhall
      , textMap = ./textMap.dhall
      }

let createTcpPort
    : forall (port : Integer) -> forall (ip : Text) -> Rule.Type
    = ./create.dhall "tcp"

let example0 =
        assert
      :     Rule.textMap (createTcpPort +29418) [ "gerrit1", "gerrit2" ]
        ===  [ Rule::{
               , port = +29418
               , protocol = Some "tcp"
               , remote_ip_prefix = Some "gerrit1"
               }
             , Rule::{
               , port = +29418
               , protocol = Some "tcp"
               , remote_ip_prefix = Some "gerrit2"
               }
             ]

in  createTcpPort
