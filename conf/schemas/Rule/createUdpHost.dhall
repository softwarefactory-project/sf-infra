let createUdpHost
    : forall (ip : Text) -> forall (port : Integer) -> ./Type.dhall
    = \(ip : Text) -> \(port : Integer) -> ./createUdpPort.dhall port ip

in  createUdpHost
