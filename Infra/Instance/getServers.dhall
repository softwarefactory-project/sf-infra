--| Get the servers of a list of instances
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let Server = ../Server/package.dhall

let getServers
    : List Instance.Type -> List Server.Type
    = ./map.dhall ../Server/Type.dhall ./getServer.dhall

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     getServers
                  [ Instance::{
                    , name = "www"
                    , connection = Connection::{ ansible_user = "centos" }
                    , server = Server::{ image = "centos" }
                    }
                  ]
            ===  [ Server::{ image = "centos", name = "www" } ]

in  getServers
