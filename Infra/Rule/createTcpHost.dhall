--| List createTcp but with the argument inversed
let createTcpHost
    : forall (ip : Text) -> forall (port : Integer) -> ./Type.dhall
    = \(ip : Text) -> \(port : Integer) -> ./createTcpPort.dhall port ip

in  createTcpHost
