let Prelude = ../Prelude.dhall

let Server = ../Server/package.dhall

let isPresent
    : ./Type.dhall -> Bool
    = \(instance : ./Type.dhall) ->
        merge
          { None = True
          , Some = \(server : Server.Type) -> Server.isPresent server
          }
          instance.server

let keepPresent
    : List ./Type.dhall -> List ./Type.dhall
    = Prelude.List.filter ./Type.dhall isPresent

in  keepPresent
