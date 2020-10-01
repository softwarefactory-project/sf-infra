--| See the `./createTcpHost.dhall` documentation
let Rule = { Type = ./Type.dhall }

let createUdpHost
    : forall (ip : Text) -> forall (port : Integer) -> Rule.Type
    = \(ip : Text) -> \(port : Integer) -> ./create.dhall "udp" port ip

in  createUdpHost
