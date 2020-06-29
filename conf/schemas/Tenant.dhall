{- An openstack project vars -}
{ Type =
    { servers : List (./Server.dhall).Type
    , images : List (./Image.dhall).Type
    , volumes : List (./Volume.dhall).Type
    , keypairs : List (./Keypair.dhall).Type
    , security_groups : List (./SecurityGroup.dhall).Type
    , networks : Optional (List (./Network.dhall).Type)
    , subnets : Optional (List (./Subnet.dhall).Type)
    , routers : Optional (List (./Router.dhall).Type)
    , image_cache_dir : Text
    }
, default =
  { servers = [] : List (./Server.dhall).Type
  , images = [] : List (./Image.dhall).Type
  , volumes = [] : List (./Volume.dhall).Type
  , keypairs = [] : List (./Keypair.dhall).Type
  , image_cache_dir = "{{ ansible_user_dir }}/image_cache"
  , networks = None (List (./Network.dhall).Type)
  , subnets = None (List (./Subnet.dhall).Type)
  , routers = None (List (./Router.dhall).Type)
  }
}
