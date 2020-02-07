{- Global configuration defaults values -}
{ sfInfraKeypair = ./files/infra_key.pub as Text
, SecurityGroups =
  [ { name = "common"
    , rules =
      [ (./schemas/Rule.dhall)::{ port = +22 }
      , (./schemas/Rule.dhall)::{ port = -1, protocol = Some "icmp" }
      ]
    }
  , { name = "web"
    , rules =
      [ (./schemas/Rule.dhall)::{ port = +80 }
      , (./schemas/Rule.dhall)::{ port = +443 }
      ]
    }
  ]
, external-network = "public"
, Flavors =
    { `1vcpu_1gb` = "1vcpu_1gb"
    , `1vcpu_2gb` = "1vcpu_2gb"
    , `1vcpu_4gb` = "v1-standard-1"
    , `2vcpus_8gb` = "v1-standard-2"
    , `4vcpus_16gb` = "v1-standard-4"
    , `4vcpus_8gb` = "4vcpus_8gb"
    , `6vcpus_12gb` = "6vcpus_12gb"
    , `8vcpus_32gb` = "v1-standard-8"
    }
}
