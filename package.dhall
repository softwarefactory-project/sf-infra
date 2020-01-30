{- The package only contains types or functions -}
let Prelude =
      env:DHALL_PRELUDE ? https://prelude.dhall-lang.org/v13.0.0/package.dhall

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

let external-network = "public"

let Image =
      { Type =
          { name : Text
          , container_format : Text
          , disk_format : Text
          , url : Text
          , checksum : Text
          }
      , default = { container_format = "bare", disk_format = "qcow2" }
      }

let Rule =
      { Type =
          { port : Integer
          , protocol : Optional Text
          , remote_ip_prefix : Optional Text
          , port_range_max : Optional Integer
          }
      , default =
          { protocol = None Text
          , remote_ip_prefix = None Text
          , port_range_max = None Integer
          }
      }

let SecurityGroups =
      [ { name = "common"
        , rules =
          [ Rule::{ port = +22 }, Rule::{ port = -1, protocol = Some "icmp" } ]
        }
      , { name = "web"
        , rules = [ Rule::{ port = +80 }, Rule::{ port = +443 } ]
        }
      ]

let DefaultSecurityGroups = [ "common", "private-monitoring" ]

let Flavors =
      { `1cpu_4gig` = "v1-standard-1"
      , `2cpus_8gig` = "v1-standard-2"
      , `4cpus_16gig` = "v1-standard-4"
      , `8cpus_32gig` = "v1-standard-8"
      }

let Server =
      { Type =
          { name : Text
          , boot_from_volume : Text
          , flavor : Text
          , floating_ip : Text
          , image : Text
          , key_name : Text
          , network : Text
          , security_groups : List Text
          , volume_size : Optional Natural
          , volumes : Optional (List Text)
          }
      , default =
          { flavor = Flavors.`2cpus_8gig`
          , floating_ip = "no"
          , image = "centos-7-1907"
          , key_name = "sf-infra-key"
          , network = "private-network"
          , security_groups = DefaultSecurityGroups
          , volume_size = None Natural
          , boot_from_volume = "yes"
          , volumes = None (List Text)
          }
      }

let sfInfraKeypair = ./files/infra_key.pub as Text

let mkNetwork =
          \(name : Text)
      ->  { name = name ++ "-network"
          , external_network = external-network
          , port_security_enabled = False
          }

let mkSubnet =
          \(name : Text)
      ->  \(network_prefix : Text)
      ->  { name = name ++ "-subnet"
          , cidr = network_prefix ++ ".0/24"
          , gateway_ip = network_prefix ++ ".1"
          , dns_nameservers = [ "1.1.1.1", "8.8.8.8" ]
          , network_name = name ++ "-network"
          }

let mkRouter =
          \(name : Text)
      ->  \(network_prefix : Text)
      ->  { name = name ++ "-router"
          , network = external-network
          , interfaces =
            [ { net = name ++ "-network"
              , subnet = name ++ "-subnet"
              , portip = network_prefix ++ ".1"
              }
            ]
          }

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

let setFqdn =
          \(fqdn : Text)
      ->  Prelude.List.map
            Server.Type
            Server.Type
            (     \(server : Server.Type)
              ->  server // { name = server.name ++ "." ++ fqdn }
            )

in  { Prelude = Prelude
    , Flavors = Flavors
    , Image = Image
    , mkServers = mkServers
    , mkSubnet = mkSubnet
    , mkNetwork = mkNetwork
    , mkRouter = mkRouter
    , sfInfraKeypair = sfInfraKeypair
    , Rule = Rule
    , DefaultSecurityGroups = DefaultSecurityGroups
    , Server = Server
    , seq = seq
    , SecurityGroups = SecurityGroups
    , setFqdn = setFqdn
    }
