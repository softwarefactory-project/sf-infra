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
          \(network : Text)
      ->  \(name : Text)
      ->  { name = name ++ "-network"
          , external_network = network
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
          \(network : Text)
      ->  \(name : Text)
      ->  \(network_prefix : Text)
      ->  { name = name ++ "-router"
          , network = network
          , port_security_enabled = None Bool
          , interfaces =
            [ { net = name ++ "-network"
              , subnet = name ++ "-subnet"
              , portip = network_prefix ++ ".1"
              }
            ]
          }

let Server = ./schemas/Server.dhall

let Volume = ./schemas/Volume.dhall

let Instance = ./schemas/Instance.dhall

let Group = ./types/Group.dhall

let Groups = ./types/Groups.dhall

let StrOrInt = < int : Natural | str : Text >

let mapServer = Prelude.List.map Server.Type Server.Type

let mapInstance = Prelude.List.map Instance.Type Instance.Type

let mkHost =
      Prelude.List.map
        Instance.Type
        { mapKey : Text
        , mapValue : List { mapKey : Text, mapValue : StrOrInt }
        }
        (     \(instance : Instance.Type)
          ->  { mapKey = instance.name
              , mapValue = toMap
                  { ansible_user = StrOrInt.str instance.connection.ansible_user
                  , ansible_python_interpreter =
                      StrOrInt.str
                        instance.connection.ansible_python_interpreter
                  , ansible_port = StrOrInt.int instance.connection.ansible_port
                  }
              }
        )

let mkGroup =
          \(instances : List Instance.Type)
      ->  let {- Convert list of instance to their name dict
              -} instanceName =
                Prelude.List.map
                  Instance.Type
                  { mapKey : Text, mapValue : {} }
                  (     \(instance : Instance.Type)
                    ->  { mapKey = instance.name, mapValue = {=} }
                  )

          let {- Check if a instance is part of a group
              -} filterInstance =
                    \(group : Groups.Type)
                ->  \(instance : Instance.Type)
                ->  Prelude.List.fold
                      Group
                      instance.groups
                      Bool
                      (     \(group : Group)
                        ->  \(acc : Bool)
                        ->  acc || group@1.test group
                      )
                      False

          let {- Create a group value for a given groupType
              -} mkGroup =
                    \(group : Groups.Type)
                ->  { mapKey = Groups.show group.value
                    , mapValue.hosts =
                        instanceName
                          ( Prelude.List.filter
                              Instance.Type
                              (filterInstance group)
                              instances
                          )
                    }

          in  Prelude.List.map
                Groups.Type
                { mapKey : Text
                , mapValue : { hosts : List { mapKey : Text, mapValue : {} } }
                }
                mkGroup
                Groups.groups

let setFqdn =
          \(fqdn : Text)
      ->  mapInstance
            (     \(instance : Instance.Type)
              ->  instance // { name = instance.name ++ "." ++ fqdn }
            )

in  { Prelude = Prelude
    , getServers =
            \(instances : List Instance.Type)
        ->  Prelude.List.map
              Instance.Type
              Server.Type
              (\(i : Instance.Type) -> i.server // { name = i.name })
              ( Prelude.List.filter
                  Instance.Type
                  (\(i : Instance.Type) -> i.skip_os_server_task == False)
                  instances
              )
    , getVolumes =
            \(instances : List Instance.Type)
        ->  Prelude.List.concat
              Volume.Type
              ( Prelude.List.map
                  Instance.Type
                  (List Volume.Type)
                  (     \(i : Instance.Type)
                    ->  Prelude.List.map
                          Volume.Type
                          Volume.Type
                          (\(v : Volume.Type) -> v // { server = i.name })
                          i.volumes
                  )
                  instances
              )
    , mkHost = mkHost
    , mkGroup = mkGroup
    , mkSubnetWithMask = mkSubnetWithMask
    , mkSubnet = mkSubnet
    , mkNetwork = mkNetwork
    , mkRouter = mkRouter
    , setSecurityGroups =
            \(security-groups : List Text)
        ->  mapInstance
              (     \(instance : Instance.Type)
                ->      instance
                    //  { server =
                                instance.server
                            //  { security_groups =
                                      security-groups
                                    # instance.server.security_groups
                                }
                        }
              )
    , setIp =
        \(ip : Text) -> { auto_ip = None Bool, floating_ips = Some [ ip ] }
    , seq = seq
    , setFqdn = setFqdn
    , mapServerText = Prelude.List.map Server.Type Text
    }
