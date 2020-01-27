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

let Subnet =
      { Type =
          { name : Text
          , cidr : Text
          , dns_nameservers : List Text
          , gateway_ip : Text
          , network_name : Text
          }
      , default =
          { name = "private-subnet"
          , dns_nameservers = [ "1.1.1.1", "8.8.8.8" ]
          , network_name = "private-network"
          }
      }

let RouterInterface =
      { Type = { net : Text, portip : Text, subnet : Text }
      , default = { net = "private-network", subnet = Subnet.default.name }
      }

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
          }
      , default = { protocol = None Text, remote_ip_prefix = None Text }
      }

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
          }
      , default =
          { boot_from_volume = "no"
          , flavor = Flavors.`2cpus_8gig`
          , floating_ip = "no"
          , image = "centos-7-1907"
          , key_name = "sf-infra-key"
          , network = "private-network"
          , security_groups = DefaultSecurityGroups
          , volume_size = None Natural
          }
      }

let sfInfraKeypair = ./files/infra_key.pub as Text

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
    , sfInfraKeypair = sfInfraKeypair
    , RouterInterface = RouterInterface
    , Rule = Rule
    , DefaultSecurityGroups = DefaultSecurityGroups
    , Server = Server
    , Subnet = Subnet
    , seq = seq
    , setFqdn = setFqdn
    }
