let getServer = ./getServer.dhall

let Server = ../Server/package.dhall

let showInstance =
      \(instance : ./Type.dhall) ->
        "* ${instance.name} " ++ Server.maybeShow (getServer instance)

in  showInstance
