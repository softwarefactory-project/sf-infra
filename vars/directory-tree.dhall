let Prelude = ../Infra/Prelude.dhall

let Infra = ../Infra/package.dhall

let vars = ../vars/package.dhall

let sshconfig =
      let mkConn =
            \(instance : Infra.Instance.Type) ->
              let optional-proxy-command =
                    merge
                      { None = None Text
                      , Some =
                          \(command : Text) -> Some ("ProxyCommand " ++ command)
                      }
                      instance.connection.proxy_command

              let optional-proxy-jump =
                    merge
                      { None = None Text
                      , Some = \(host : Text) -> Some ("ProxyJump " ++ host)
                      }
                      instance.connection.proxy_jump

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
                              [ optional-proxy-command
                              , optional-proxy-jump
                              , optional-hostname
                              ]
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
                  ControlPath ~/.ssh/control-%r@%h:%p
                  PubkeyAcceptedKeyTypes +ssh-rsa

              Host 192.168.25.10
                  ProxyJump baremetal03.rdoproject.org
              Host 192.168.25.11
                  ProxyJump baremetal03.rdoproject.org
              Host 192.168.25.12
                  ProxyJump baremetal03.rdoproject.org
              Host 192.168.25.13
                  ProxyJump baremetal03.rdoproject.org
              Host 192.168.26.10
                  ProxyJump baremetal04.rdoproject.org
              Host 192.168.26.11
                  ProxyJump baremetal04.rdoproject.org
              Host 192.168.26.12
                  ProxyJump baremetal04.rdoproject.org
              Host 192.168.26.13
                  ProxyJump baremetal04.rdoproject.org


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

in  { doc.`inventory.md`
      = header ++ Infra.Instance.mkMarkdownInventory vars.instances
    , roles.system.generate-etc-hosts.files.sshconfig = header ++ sshconfig
    }
