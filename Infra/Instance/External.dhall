--| Adds an empty server and indicate this instance is not managed
let Server = ../Server/package.dhall

let External =
      { skip_os_server_task = True, server = Server::{ image = "unknown" } }

let example0 =
      let Instance = { Type = ./Type.dhall, default = ./default.dhall }

      let Connection = ../Connection/package.dhall

      in    assert
          :     Instance::(     { connection = Connection::{
                                  , ansible_user = "centos"
                                  }
                                , name = "www"
                                }
                            //  External
                          )
            ===  Instance::{
                 , connection = Connection::{ ansible_user = "centos" }
                 , name = "www"
                 , skip_os_server_task = True
                 , server = Server::{ image = "unknown" }
                 }

in  External
