--| A Rule constructor
let Rule = { Type = ./Type.dhall, default = ./default.dhall }

let create
    : forall (protocol : Text) ->
      forall (port : Integer) ->
      forall (ip : Text) ->
        Rule.Type
    = \(proto : Text) ->
      \(port : Integer) ->
      \(ip : Text) ->
        Rule::{ port, protocol = Some proto, remote_ip_prefix = Some ip }

let example0 =
        assert
      :     create "tcp" +8080 "10.0.0.2"
        ===  Rule::{
             , protocol = Some "tcp"
             , port = +8080
             , remote_ip_prefix = Some "10.0.0.2"
             }

in  create
