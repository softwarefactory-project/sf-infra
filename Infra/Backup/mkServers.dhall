let Prelude = ../Prelude.dhall

let Backup = { Type = ./Type.dhall, default = ./default.dhall }

let Server = ./Server/package.dhall

let mkServers =
      \(Configs : List Backup.Type) ->
        Prelude.List.map
          Backup.Type
          Server.Type
          ( \(server : Backup.Type) ->
              let getOptional =
                    \(type : Type) ->
                    \(data : Optional type) ->
                      merge
                        { Some = \(x : type) -> Some x, None = None type }
                        data

              let hostname =
                    Prelude.Optional.default
                      Text
                      server.hostname
                      server.real_name

              in  Server::{
                  , filename = hostname ++ ".yaml"
                  , instances =
                      merge
                        { None = Some [ hostname ]
                        , Some = \(instances : List Text) -> server.instances
                        }
                        server.instances
                  , hour = server.hour
                  , remote_dir = getOptional Text server.remote_dir
                  , run_sf_backup = server.run_sf_backup
                  , www_dir = getOptional Text server.www_dir
                  , playbook = getOptional Text server.playbook
                  , sf_releases = getOptional (List Text) server.sf_releases
                  , dir = getOptional Text server.dir
                  , domain = getOptional Text server.domain
                  , month_subdir = getOptional Natural server.month_subdir
                  }
          )
          Configs

in  mkServers
