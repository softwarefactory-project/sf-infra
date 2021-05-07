let Prelude = ../Prelude.dhall

let Backup = { Type = ./Type.dhall, default = ./default.dhall }

let Location = ./Location/package.dhall

let mkLocations =
      \(Configs : List Backup.Type) ->
        Prelude.List.map
          Backup.Type
          Location.Type
          ( \(server : Backup.Type) ->
              let hostname =
                    Prelude.Optional.default
                      Text
                      server.hostname
                      server.real_name

              in  Location::{
                  , dir = "/var/lib/backup/bup/${hostname}"
                  , domain = hostname
                  }
          )
          Configs

in  mkLocations
