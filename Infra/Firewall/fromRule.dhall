--| This function creates a firewall rule from a security group rule
let Firewall = { Type = ./Type.dhall, default = ./default.dhall }

let Rule = ../Rule/package.dhall

let fromRule
    : Rule.Type -> Firewall.Type
    = \(rule : Rule.Type) ->
        let proto =
              merge
                { None = "tcp", Some = \(proto : Text) -> proto }
                rule.protocol

        let port = Integer/clamp rule.port

        in  merge
              { None = Firewall::{ port = Some "${Natural/show port}/${proto}" }
              , Some =
                  \(address : Text) ->
                    Firewall::{
                    , rich_rule = Some
                        (     "rule family=ipv4 "
                          ++  "source address=${address} "
                          ++  "port port=${Natural/show port} "
                          ++  "protocol=${proto} accept"
                        )
                    }
              }
              rule.remote_ip_prefix

let example0 =
        assert
      : fromRule Rule::{ port = +8080 } === Firewall::{ port = Some "8080/tcp" }

let example1 =
        assert
      :     fromRule Rule::{ port = +8080, remote_ip_prefix = Some "10.0.0.1" }
        ===  Firewall::{
             , rich_rule = Some
                 (     "rule family=ipv4 "
                   ++  "source address=10.0.0.1 "
                   ++  "port port=8080 "
                   ++  "protocol=tcp accept"
                 )
             }

in  fromRule
