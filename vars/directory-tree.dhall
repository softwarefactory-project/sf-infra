let Prelude = ../Infra/Prelude.dhall

let Infra = ../Infra/package.dhall

let vars = ../vars/package.dhall

let sshconfig =
      let mkConn =
            \(instance : Infra.Instance.Type) ->
              let optional-proxy =
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

let README =
      ''
      Operational Playbook for softwarefactory-project.io
      ===================================================

      # Inventory

      This project manages:

      ${Inventory}

      # Jobs

      There are a few jobs:

      * sf-infra-create-bridge creates the bridge, and it is the only one running inside a container
      * sf-infra-configure-tenants setup openstack tenant (running from the bridge)
      * sf-infra-create-hosts creates new host and display their IP
      * sf-infra-configure-hosts run the site.yaml playbook

      The idea is to have openstacksdk tasks in jobs that are running only when needed.
      Then most of the work is done with the configure-hosts job that use a static inventory.

      # To modify the openstack resources managed by sf-infra:

      Edit the files in the top-level vars directory, for example:

      * Modifies instances to vars/infra-rdo/instances.dhall
      * Update network configuration in vars/*/networking.dhall

      Then run `make` to update the yaml files.

      To manage the configuration, use an existing group such as `monitoring` and/or add an entry in the `playbooks/site.yaml`

      Use git-review to submit the change and let the CI create and run the playbook.

      # To add a vault secret

      From the bridge, fedora account:
      ```
      ansible-vault encrypt_string --stdin-name var-name < file-var-value >> var-file.yaml
      ```

      # To update playbook vars

      The variable are now declared using dhall. Run `make` to update the yaml files.
      Have a look to doc/dhall-onboarding/README.md to get started with dhall
      ''

in  { `README.md` = README
    , roles.generate-etc-hosts.files.sshconfig = header ++ sshconfig
    }
