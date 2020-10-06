--| Update the server of an instance
let Server = ../Server/package.dhall

let Instance = { Type = ./Type.dhall }

let updateServer
    : forall (update : Server.Type -> Server.Type) ->
      forall (instance : Instance.Type) ->
        Instance.Type
    = \(update : Server.Type -> Server.Type) ->
      \(instance : Instance.Type) ->
        merge
          { None = instance
          , Some =
              \(server : Server.Type) ->
                instance // { server = Some (update server) }
          }
          instance.server

in  updateServer
