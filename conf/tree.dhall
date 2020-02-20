let Infra = ../package.dhall

let servers = Infra.SF.servers # Infra.RDO.servers

let sshconfig =
      let mkConn =
                \(server : Infra.Server.Type)
            ->  ''
                Host ${server.name}
                    User ${server.ansible_user}
                    Port ${Natural/show server.ansible_port}

                ''

      in      ''
              Host *
                  ControlMaster auto
                  ControlPath /run/user/1000/ssh/%r@%h:%p

              ''
          ++  Infra.Prelude.Text.concat
                (Infra.Prelude.List.map Infra.Server.Type Text mkConn servers)

let header =
      ''
      # This file is managed by dhall.
      ''

in  { roles.generate-etc-hosts.files.sshconfig = header ++ sshconfig }
