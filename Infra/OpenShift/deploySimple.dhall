{-| DeploySimple create simple resources for a service defined by:

- name: the service name
- image: the container image reference
- port: the tcp network port number
- hostname: the desired route hostname
- config: the service ConfigMap
-}
let OpenShift = ./Import.dhall

let DeploySimple =
      \(name : Text) ->
      \(image : Text) ->
      \(port : Natural) ->
      \(hostname : Text) ->
      \(config : OpenShift.ConfigMap.Type) ->
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
                    , volumes = Some
                      [ OpenShift.Volume::{
                        , name = "${name}-config"
                        , configMap = Some OpenShift.ConfigMapVolumeSource::{
                          , name = config.metadata.name
                          }
                        }
                      ]
                    , containers =
                      [ OpenShift.Container::{
                        , name
                        , image = Some image
                        , ports = Some
                          [ OpenShift.ContainerPort::{ containerPort = port } ]
                        , volumeMounts = Some
                          [ OpenShift.VolumeMount::{
                            , name = "${name}-config"
                            , mountPath = "/data/"
                            }
                          ]
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

        in  [ OpenShift.Resource.Route route
            , OpenShift.Resource.Service service
            , OpenShift.Resource.Deployment deployment
            , OpenShift.Resource.ConfigMap config
            ]

in  DeploySimple
