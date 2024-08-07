--| This functions add security groups and set the fqdn of a list of instances
let Instance = ../Instance/package.dhall

let Server = ../Server/package.dhall

let addSecurityGroupsAndSetFqdn
    : forall (security-groups : List Text) ->
      forall (fqdn : Text) ->
      List Instance.Type ->
        List Instance.Type
    = \(security-groups : List Text) ->
      \(fqdn : Text) ->
      \(instances : List Instance.Type) ->
        Instance.mapInstance
          (Instance.updateServer (Server.addSecurityGroups security-groups))
          (Instance.mapInstance (Instance.setFqdn fqdn) instances)

let example0 =
        assert
      :     addSecurityGroupsAndSetFqdn
              [ "monitoring" ]
              "softwarefactory-project.io"
              [ Instance::{
                , name = "www"
                , server = Some Server::{ image = "centos" }
                }
              ]
        ===  [ Instance::{
               , name = "www.softwarefactory-project.io"
               , server = Some Server::{
                 , image = "centos"
                 , security_groups = [ "monitoring" ]
                 }
               }
             ]

in  addSecurityGroupsAndSetFqdn
