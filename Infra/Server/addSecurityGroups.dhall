--| Add security groups to a server
let Server = { Type = ./Type.dhall, default = ./default.dhall }

let addSecurityGroups
    : forall (security-groups : List Text) -> Server.Type -> Server.Type
    = \(security-groups : List Text) ->
      \(server : ./Type.dhall) ->
        server // { security_groups = security-groups # server.security_groups }

let example0 =
        assert
      :     addSecurityGroups [ "monitoring" ] Server::{ image = "centos" }
        ===  Server::{ image = "centos", security_groups = [ "monitoring" ] }

in  addSecurityGroups
