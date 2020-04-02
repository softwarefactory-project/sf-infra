let Infra = ../package.dhall

let sshconfig =
      let mkConn =
                \(instance : Infra.Instance.Type)
            ->  let optional-proxy =
                      merge
                        { None = ""
                        , Some =
                            \(command : Text) -> "    ProxyCommand " ++ command
                        }
                        instance.connection.proxy_command

                in  ''
                    Host ${instance.name}
                        User ${instance.connection.ansible_user}
                        Port ${Natural/show instance.connection.ansible_port}
                    ${optional-proxy}
                    ''

      in      ''
              Host *
                  ControlMaster auto
                  ControlPath /run/user/1000/%r@%h:%p

              ''
          ++  Infra.Prelude.Text.concat
                ( Infra.Prelude.List.map
                    Infra.Instance.Type
                    Text
                    mkConn
                    Infra.instances
                )

let header =
      ''
      # This file is managed by the ./vars/directory-tree.dhall file.
      ''

in  { roles.generate-etc-hosts.files.sshconfig = header ++ sshconfig }
