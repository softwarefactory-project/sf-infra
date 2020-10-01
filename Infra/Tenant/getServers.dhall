--| Returns the list of created instances
let Instance = ../Instance/package.dhall

let Server = ../Server/package.dhall

let getServers
    : List Instance.Type -> List Server.Type
    = \(instances : List Instance.Type) ->
        Instance.getServers (Instance.filter Instance.isCreated instances)

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     getServers
                  [ Instance::{
                    , connection = Connection::{ ansible_user = "centos" }
                    , name = "www"
                    , server = Server::{ image = "centos" }
                    }
                  ]
            ===  [ Server::{ name = "www", image = "centos" } ]

in  getServers
