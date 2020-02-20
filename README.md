Operational Playbook for softwarefactory-project.io
===================================================

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
