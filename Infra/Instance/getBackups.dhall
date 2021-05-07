--| Get the backups of a list of instances
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let Backup = ../Backup/package.dhall

let Prelude = ../Prelude.dhall

let getBackups
    : List Instance.Type -> List Backup.Type
    = \(instances : List Instance.Type) ->
        Prelude.List.unpackOptionals
          Backup.Type
          ( ./map.dhall
              (Optional ../Backup/Type.dhall)
              ./getBackup.dhall
              instances
          )

let example0 =
      let Connection = ../Connection/package.dhall

      in    assert
          :     getBackups
                  [ Instance::{
                    , name = "centos"
                    , connection = Connection::{ ansible_user = "centos" }
                    , backup = Some Backup::{ run_sf_backup = True }
                    }
                  , Instance::{
                    , name = "fedora"
                    , connection = Connection::{ ansible_user = "fedora" }
                    , backup = Some Backup::{
                      , run_sf_backup = False
                      , playbook = Some "myplay.yaml"
                      }
                    }
                  ]
            ===  [ Backup::{ hostname = "centos", run_sf_backup = True }
                 , Backup::{
                   , hostname = "fedora"
                   , run_sf_backup = False
                   , playbook = Some "myplay.yaml"
                   }
                 ]

in  getBackups
