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
        assert
      :     getBackups
              [ Instance::{
                , name = "centos"
                , backup = Some Backup::{ run_sf_backup = True }
                }
              , Instance::{
                , name = "fedora"
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
