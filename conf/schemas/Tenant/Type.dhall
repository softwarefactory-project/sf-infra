{ image_cache_dir : Text
, images : List ../Image/Type.dhall
, keypairs : List ../Keypair/Type.dhall
, networks : Optional (List ../Network/Type.dhall)
, routers : Optional (List ../Router/Type.dhall)
, security_groups : List ../SecurityGroup/Type.dhall
, servers : List ../Server/Type.dhall
, subnets : Optional (List ../Subnet/Type.dhall)
, volumes : List ../Volume/Type.dhall
}
