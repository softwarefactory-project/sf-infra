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

let Group = ./types/Group.dhall

let Groups = ./types/Groups.dhall

let mkGroup =
          \(servers : List Server.Type)
      ->  let {- Convert list of server to their name dict
              -} serverName =
                Prelude.List.map
                  Server.Type
                  { mapKey : Text, mapValue : {} }
                  (     \(server : Server.Type)
                    ->  { mapKey = server.name, mapValue = {=} }
                  )

          let {- Check if a server is part of a group
              -} filterServer =
                    \(group : Groups.Type)
                ->  \(server : Server.Type)
                ->  merge
                      { None = False
                      , Some =
                              \(groups : List Group)
                          ->  Prelude.List.fold
                                Group
                                groups
                                Bool
                                (     \(group : Group)
                                  ->  \(acc : Bool)
                                  ->  acc || group@1.test group
                                )
                                False
                      }
                      server.groups

          let {- Create a group value for a given groupType
              -} mkGroup =
                    \(group : Groups.Type)
                ->  { mapKey = Groups.show group.value
                    , mapValue =
                        { hosts =
                            serverName
                              ( Prelude.List.filter
                                  Server.Type
                                  (filterServer group)
                                  servers
                              )
                        }
                    }

          in  Prelude.List.map
                Groups.Type
                { mapKey : Text
                , mapValue : { hosts : List { mapKey : Text, mapValue : {} } }
                }
                mkGroup
                Groups.groups

let StrOrInt = ./types/StrOrInt.dhall

let mkHost =
      Prelude.List.map
        Server.Type
        { mapKey : Text
        , mapValue : List { mapKey : Text, mapValue : StrOrInt }
        }
        (     \(server : Server.Type)
          ->  { mapKey = server.name
              , mapValue =
                    server.host_vars
                  # toMap
                      { ansible_user = StrOrInt.str server.ansible_user
                      , ansible_python_interpreter =
                          StrOrInt.str server.ansible_python_interpreter
                      , ansible_port = StrOrInt.int server.ansible_port
                      }
              }
        )

let mkServers =
          \(name : Text)
      ->  \(flavor : Text)
      ->  \(count : Natural)
      ->  Prelude.List.map
            Natural
            Server.Type
            (     \(idx : Natural)
              ->  Server::(     { name = "${name}0${Natural/show idx}"
                                , flavor = flavor
                                , boot_from_volume = "no"
                                }
                            //  (./defaults.dhall).OS.CentOS.`7.0`
                          )
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
        , mkGroup = mkGroup
        , mkHost = mkHost
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
            \(ip : Text) -> { auto_ip = None Bool, floating_ips = Some [ ip ] }
        , seq = seq
        , setFqdn = setFqdn
        , mapServerText = Prelude.List.map Server.Type Text
        , StrOrInt = StrOrInt
        }
    //  ./schemas.dhall
    //  ./defaults.dhall
    //  { Group = ./types/Group.dhall, Groups = ./types/Groups.dhall }
