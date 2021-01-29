{-| DeploySimple create simple resources for a service defined by:

- name: the service name
- image: the container image reference
- port: the tcp network port number
- hostname: the desired route hostname
- config: the service ConfigMap
-}
let OpenShift = ./Import.dhall

let deploySimple =
      \(name : Text) ->
      \(image : Text) ->
      \(port : Natural) ->
      \(hostname : Text) ->
      \(config : OpenShift.ConfigMap.Type) ->
        ./deploy.dhall
          name
          image
          port
          hostname
          (None (List { mapKey : Text, mapValue : Text }))
          (Some config)

in  deploySimple
