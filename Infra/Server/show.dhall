let show =
      \(server : ./Type.dhall) ->
            "${server.image} "
        ++  merge { None = "", Some = \(flavor : Text) -> flavor } server.flavor

let maybeShow =
      \(maybeServer : Optional ./Type.dhall) ->
        merge
          { Some = \(server : ./Type.dhall) -> show server, None = "" }
          maybeServer

in  { show, maybeShow }
