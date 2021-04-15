let Prelude = ../Prelude.dhall

let Backup = { Type = ./Type.dhall, default = ./default.dhall }

let hour = 0

let mkCron =
      \(servers : List { index : Natural, value : Backup.Type }) ->
        Prelude.List.map
          { index : Natural, value : Backup.Type }
          Backup.Type
          ( \(server : { index : Natural, value : Backup.Type }) ->
              server.value // { hour = hour + server.index }
          )
          servers

in  mkCron
