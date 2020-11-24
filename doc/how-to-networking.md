# Networking

Network configuration for project is done on `vars/$project/networking.dhall`.
Some security groups used in more than one project are defined on
`vars/common.dhall`.

The variables needed by ansible modules for networking are generated using
networking.dhall files:
* networks
* subnets
* routers
* security groups and security group rules
* keypairs

All the dhall types for networking are defined on Infra directory.

To add a security group and open the port 6000 on tcp (default), edit the `networking.dhall` file for the needed project:
```dhall
{ name = "my_sec_group"
, rules = [ Infra.Rule::{ port = +6000 } ]
}
```

All the options for rule are defined on `Infra/Rule/Type.dhall`

Then run `make` from the root directory to update the yaml files and use
git-review to submit the change and let the CI run the deployment and
configuration playbooks.
