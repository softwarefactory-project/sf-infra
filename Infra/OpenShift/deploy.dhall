{-| DeploySimple create simple resources for a service defined by:

- name: the service name
- image: the container image reference
- port: the tcp network port number
- hostname: the desired route hostname
- config: the service ConfigMap
-}
let OpenShift = ./Import.dhall

let Prelude = ../Prelude.dhall

let deploy =
      \(name : Text) ->
      \(image : Text) ->
      \(port : Natural) ->
      \(hostname : Text) ->
      \(env : Optional (List { mapKey : Text, mapValue : Text })) ->
      \(config : Optional OpenShift.ConfigMap.Type) ->
        let envToEnvVar =
              let convert =
                    \(env : { mapKey : Text, mapValue : Text }) ->
                      OpenShift.EnvVar::{
                      , name = env.mapKey
                      , value = Some env.mapValue
                      }

              in  Prelude.List.map
                    { mapKey : Text, mapValue : Text }
                    OpenShift.EnvVar.Type
                    convert

        let deployment =
              OpenShift.Deployment::{
              , metadata = OpenShift.ObjectMeta::{ name = Some name }
              , spec = Some OpenShift.DeploymentSpec::{
                , selector = OpenShift.LabelSelector::{
                  , matchLabels = Some (toMap { app = name })
                  }
                , replicas = Some 1
                , template = OpenShift.PodTemplateSpec::{
                  , metadata = OpenShift.ObjectMeta::{
                    , name = Some name
                    , labels = Some (toMap { app = name })
                    }
                  , spec = Some OpenShift.PodSpec::{
                    , volumes =
                        merge
                          { None = None (List OpenShift.Volume.Type)
                          , Some =
                              \(config : OpenShift.ConfigMap.Type) ->
                                Some
                                  [ OpenShift.Volume::{
                                    , name = "${name}-config"
                                    , configMap = Some OpenShift.ConfigMapVolumeSource::{
                                      , name = config.metadata.name
                                      }
                                    }
                                  ]
                          }
                          config
                    , containers =
                      [ OpenShift.Container::{
                        , name
                        , env =
                            merge
                              { None = None (List OpenShift.EnvVar.Type)
                              , Some =
                                  \ ( env
                                    : List { mapKey : Text, mapValue : Text }
                                    ) ->
                                    Some (envToEnvVar env)
                              }
                              env
                        , image = Some image
                        , ports = Some
                          [ OpenShift.ContainerPort::{ containerPort = port } ]
                        , volumeMounts =
                            merge
                              { None = None (List OpenShift.VolumeMount.Type)
                              , Some =
                                  \(config : OpenShift.ConfigMap.Type) ->
                                    Some
                                      [ OpenShift.VolumeMount::{
                                        , name = "${name}-config"
                                        , mountPath = "/data/"
                                        }
                                      ]
                              }
                              config
                        }
                      ]
                    }
                  }
                }
              }

        let service =
              OpenShift.Service::{
              , metadata = OpenShift.ObjectMeta::{
                , name = Some "${name}-service"
                }
              , spec = Some OpenShift.ServiceSpec::{
                , selector = Some (toMap { app = name })
                , ports = Some
                  [ OpenShift.ServicePort::{
                    , targetPort = Some (OpenShift.IntOrString.Int port)
                    , port = 80
                    }
                  ]
                }
              }

        let route =
              OpenShift.Route::{
              , metadata = OpenShift.ObjectMeta::{ name = Some name }
              , spec = OpenShift.RouteSpec::{
                , host = hostname
                , path = Some "/"
                , port = Some OpenShift.RoutePort::{
                  , targetPort = OpenShift.IntOrString.Int port
                  }
                , tls = Some OpenShift.TLSConfig::{
                  , insecureEdgeTerminationPolicy = Some "Redirect"
                  , termination = "edge"
                  }
                , to = OpenShift.RouteTargetReference::{
                  , kind = "Service"
                  , name = "${name}-service"
                  , weight = 100
                  }
                }
              }

        in    [ OpenShift.Resource.Route route
              , OpenShift.Resource.Service service
              , OpenShift.Resource.Deployment deployment
              ]
            # merge
                { None = [] : List OpenShift.Resource
                , Some =
                    \(config : OpenShift.ConfigMap.Type) ->
                      [ OpenShift.Resource.ConfigMap config ]
                }
                config

in  deploy
