--| DeployWithEnv create simple resources for a service
let OpenShift = ./Import.dhall

let deployWithEnv =
      \(name : Text) ->
      \(image : Text) ->
      \(port : Natural) ->
      \(hostname : Text) ->
      \(env : List { mapKey : Text, mapValue : Text }) ->
        ./deploy.dhall
          name
          image
          port
          hostname
          (Some env)
          (None OpenShift.ConfigMap.Type)

in  deployWithEnv
