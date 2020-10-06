--| Get the servers of a list of instances
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let Server = ../Server/package.dhall

let Prelude = ../Prelude.dhall

let getServers
    : List Instance.Type -> List Server.Type
    = \(instances : List Instance.Type) ->
        Prelude.List.unpackOptionals
          Server.Type
          ( ./map.dhall
              (Optional ../Server/Type.dhall)
              ./getServer.dhall
              instances
          )

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     getServers
                  [ Instance::{
                    , name = "www"
                    , connection = Connection::{ ansible_user = "centos" }
                    , server = Some Server::{ image = "centos" }
                    }
                  ]
            ===  [ Server::{ image = "centos", name = "www" } ]

in  getServers
