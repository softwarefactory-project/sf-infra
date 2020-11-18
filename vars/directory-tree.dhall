let Prelude = ../Infra/Prelude.dhall

let Infra = ../Infra/package.dhall

let vars = ../vars/package.dhall

let sshconfig =
      let mkConn =
            \(instance : Infra.Instance.Type) ->
              let optional-proxy =
                    merge
                      { None = None Text
                      , Some =
                          \(command : Text) -> Some ("ProxyCommand " ++ command)
                      }
                      instance.connection.proxy_command

              let optional-hostname =
                    merge
                      { None = None Text
                      , Some = \(hostip : Text) -> Some ("Hostname " ++ hostip)
                      }
                      instance.connection.ansible_host

              let indent =
                    Prelude.List.map Text Text (\(n : Text) -> "    " ++ n)

              let extra =
                    Prelude.Text.concatSep
                      "\n"
                      ( indent
                          ( Prelude.List.unpackOptionals
                              Text
                              [ optional-proxy, optional-hostname ]
                          )
                      )

              in  ''
                  Host ${instance.name}
                      User ${instance.connection.ansible_user}
                      Port ${Natural/show instance.connection.ansible_port}
                  ${extra}
                  ''

      in      ''
              Host *
                  ControlMaster auto
                  ControlPath /run/user/1000/%r@%h:%p

              ''
          ++  Prelude.Text.concat
                ( Prelude.List.map
                    Infra.Instance.Type
                    Text
                    mkConn
                    vars.instances
                )

let header =
      ''
      # This file is managed by the ./vars/directory-tree.dhall file.
      ''

let Inventory =
      Prelude.Text.concatSep
        "\n"
        ( Prelude.List.map
            Infra.Instance.Type
            Text
            (\(instance : Infra.Instance.Type) -> "* ${instance.name}")
            vars.instances
        )

let inventory =
      ''
      # Inventory

      This project manages:

      ${Inventory}

      ''

in  { doc.`inventory.md` = inventory
    , roles.generate-etc-hosts.files.sshconfig = header ++ sshconfig
    }
