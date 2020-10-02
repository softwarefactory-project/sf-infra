{ image_cache_dir = "{{ ansible_user_dir }}/image_cache"
, images = [] : List ../Image/Type.dhall
, keypairs = [] : List ../Keypair/Type.dhall
, networks = None (List ../Network/Type.dhall)
, routers = None (List ../Router/Type.dhall)
, servers = [] : List ../Server/Type.dhall
, subnets = None (List ../Subnet/Type.dhall)
, volumes = [] : List ../Volume/Type.dhall
}
