--| Returns the list of created instances
let Instance = ../Instance/package.dhall

let Server = ../Server/package.dhall

let getServers
    : List Instance.Type -> List Server.Type
    = \(instances : List Instance.Type) ->
        Instance.getServers (Instance.filter Instance.isCreated instances)

let example0 =
        assert
      :     getServers
              [ Instance::{
                , name = "www"
                , server = Some Server::{ image = "centos" }
                }
              ]
        ===  [ Server::{ name = "www", image = "centos" } ]

in  getServers
