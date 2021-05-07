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
      let Connection = ../Connection/package.dhall

      in    assert
          :     getBackup
                  Instance::{
                  , name = "www"
                  , connection = Connection::{ ansible_user = "centos" }
                  , backup = Some Backup::{ run_sf_backup = True }
                  }
            ===  Some Backup::{ run_sf_backup = True, hostname = "www" }

in  getBackup
