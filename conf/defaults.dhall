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
    { `1cpu_4gig` = "v1-standard-1"
    , `2cpus_8gig` = "v1-standard-2"
    , `4cpus_16gig` = "v1-standard-4"
    , `8cpus_32gig` = "v1-standard-8"
    }
}
