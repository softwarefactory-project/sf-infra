--| Return the server name
let Server = { Type = ./Type.dhall, default = ./default.dhall }

let getName
    : Server.Type -> Text
    = \(server : ./Type.dhall) -> server.name

let example0 =
      assert : getName Server::{ image = "centos", name = "www" } === "www"

in  getName
