let Infra = ../../conf/infra.dhall

in  { images = [] : List Infra.Image.Type
    , volumes = [] : List { size : Natural }
    , servers = [] : List Infra.Server.Type
    , image_cache_dir = "{{ ansible_user_dir }}/image_cache"
    , keypairs = [] : List { public_key : Text }
    , security_groups =
      [ { name = "default"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          ]
        }
      ]
    , networks = [ Infra.mkNetwork "private" ]
    , subnets = [ Infra.mkSubnet "private" "192.168.1" ]
    , routers = [ Infra.mkRouter "private" "192.168.1" ]
    }
