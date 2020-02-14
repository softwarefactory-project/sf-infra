{- An openstack project vars -}
{ Type =
    { servers : List (./Server.dhall).Type
    , images : List (./Image.dhall).Type
    , volumes : List (./Volume.dhall).Type
    , keypairs : List (./Keypair.dhall).Type
    , security_groups : List (./SecurityGroup.dhall).Type
    , networks : List (./Network.dhall).Type
    , subnets : List (./Subnet.dhall).Type
    , routers : List (./Router.dhall).Type
    , image_cache_dir : Text
    }
, default =
    { servers = [] : List (./Server.dhall).Type
    , images = [] : List (./Image.dhall).Type
    , volumes = [] : List (./Volume.dhall).Type
    , keypairs = [] : List (./Keypair.dhall).Type
    , image_cache_dir = "{{ ansible_user_dir }}/image_cache"
    }
}
