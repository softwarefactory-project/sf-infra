{- The package contains common symbol shared by the other dhall files -}
let Prelude =
        env:DHALL_PRELUDE
      ? https://prelude.dhall-lang.org/v13.0.0/package.dhall sha256:4aa8581954f7734d09b7b21fddbf5d8df901a44b54b4ef26ea71db92de0b1a12

let {- Generate sequence like unix seq command -} seq =
          \(count : Natural)
      ->  let seq = Prelude.List.replicate count Natural 1

          let indexed = Prelude.List.indexed Natural seq

          let IndexedType = { index : Natural, value : Natural }

          in  Prelude.List.map
                IndexedType
                Natural
                (\(index : IndexedType) -> index.index + 1)
                indexed

let seq-test = assert : seq 5 === [ 1, 2, 3, 4, 5 ]

let mkNetwork =
          \(name : Text)
      ->  { name = name ++ "-network"
          , external_network = (./defaults.dhall).external-network
          , port_security_enabled = False
          }

let mkSubnetWithMask =
          \(mask : Text)
      ->  \(name : Text)
      ->  \(network_prefix : Text)
      ->  { name = name ++ "-subnet"
          , cidr = network_prefix ++ ".0/" ++ mask
          , gateway_ip = network_prefix ++ ".1"
          , dns_nameservers = [ "1.1.1.1", "8.8.8.8" ]
          , network_name = name ++ "-network"
          }

let mkSubnet = mkSubnetWithMask "24"

let mkRouter =
          \(name : Text)
      ->  \(network_prefix : Text)
      ->  { name = name ++ "-router"
          , network = (./defaults.dhall).external-network
          , port_security_enabled = None Bool
          , interfaces =
            [ { net = name ++ "-network"
              , subnet = name ++ "-subnet"
              , portip = network_prefix ++ ".1"
              }
            ]
          }

let Server = ./schemas/Server.dhall

let mkServers =
          \(name : Text)
      ->  \(flavor : Text)
      ->  \(count : Natural)
      ->  Prelude.List.map
            Natural
            Server.Type
            (     \(idx : Natural)
              ->  Server::{
                  , name = "${name}0${Natural/show idx}"
                  , flavor = flavor
                  , boot_from_volume = "no"
                  }
            )
            (seq count)

let mapServer = Prelude.List.map Server.Type Server.Type

let setFqdn =
          \(fqdn : Text)
      ->  mapServer
            (     \(server : Server.Type)
              ->  server // { name = server.name ++ "." ++ fqdn }
            )

in      { Prelude = Prelude
        , mkServers = mkServers
        , mkSubnetWithMask = mkSubnetWithMask
        , mkSubnet = mkSubnet
        , mkNetwork = mkNetwork
        , mkRouter = mkRouter
        , setSecurityGroups =
                \(security-groups : List Text)
            ->  mapServer
                  (     \(server : Server.Type)
                    ->      server
                        //  { security_groups =
                                security-groups # server.security_groups
                            }
                  )
        , setIp =
                \(ip : Text)
            ->  \(server : Server.Type)
            ->  server // { auto_ip = None Bool, floating_ips = Some [ ip ] }
        , seq = seq
        , setFqdn = setFqdn
        , mapServerText = Prelude.List.map Server.Type Text
        }
    //  ./schemas.dhall
    //  ./defaults.dhall