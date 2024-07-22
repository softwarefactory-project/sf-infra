--| Set the name and return the backup of an instance
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let Backup = ../Backup/package.dhall

let getBackup
    : Instance.Type -> Optional Backup.Type
    = \(instance : ./Type.dhall) ->
        merge
          { None = None Backup.Type
          , Some =
              \(backup : Backup.Type) ->
                Some (backup // { hostname = instance.name })
          }
          instance.backup

let example0 =
        assert
      :     getBackup
              Instance::{
              , name = "www"
              , backup = Some Backup::{ run_sf_backup = True }
              }
        ===  Some Backup::{ run_sf_backup = True, hostname = "www" }

in  getBackup
