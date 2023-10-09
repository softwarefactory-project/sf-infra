# Managing instances

## Add instance

In sf-infra, an instance is a composition of:
* the instance name
* an optional server definition (created with os_server module)
* an optional volume definition (to attach external volume to a server, created
  with os_volume)
* a connection (how to connect to the server)
* a list of ansible groups
* some options for monitoring (node-exporter installation and configuration,
  monitoring_auth_urls and monitoring_urls)

### Hints

* The dhall type and default are define on the schema directory (eg:
  `Infra/Instance/{Type,default}.dhall`)
* Online dhall schemas documentation is available on
  https://docs.softwarefactory-project.io/dhall-infra/index.html
* The images and flavors are define on `vars/common.dhall`
* security groups are define on vars/$project/networking.dhall

Instances are defined on `vars/$project/instances.dhall` files

To add an instance deployed with sf-infra jobs:

 ```dhall
Instance::{
, name = "my_instance"
, groups = [ "my_ansible_group" ]
, connection = OS.CentOS.`7.0`.connection
, server = Some Infra.Server::{
  , image = OS.CentOS.`7.0`.image.name
  , auto_ip = Some True
  , security_groups = [ "my_sec_group" ]
  , volume_size = Some 50
  }
}
 ```

To add an existing instance not deployed with sf-infra, do not add the `server`
part

 ```dhall
Instance::{
, name = "my_external_instance"
, groups = [ "my_ansible_group" ]
, connection = Infra.Connection::{
  , ansible_user = "my_user"
  }
}
 ```

The infra ssh pubkey `vars/files/infra_key.pub` should be added on the
authorized_keys for the ansible_user defined on the instance.

Then from the root directory, execute `make` to update the yaml files. This command will generate:
* ansible/hosts.yaml: ansible inventory
* doc/inventory.md: inventory documentation
* monitoring/prometheus.yaml: to disable set the `node-exporter` instance attribute to False
* playbooks/vars/$project.yaml: the ansible variables file for the project to deploy the instance
* roles/system/generate-etc-hosts/files/sshconfig: the ssh_config file to connect from the deployment host to the instance

## Configure instance

To manage the configuration, use an existing group such as `monitoring` and/or add an entry in the `playbooks/site.yaml`

## Submit the change

Use git-review to submit the change and let the CI run the deployment and configuration playbooks.
