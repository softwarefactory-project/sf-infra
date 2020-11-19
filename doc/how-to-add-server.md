# Add server

Edit the files in the top-level vars directory, for example:

* Modifies instances to vars/infra-rdo/instances.dhall
* Update network configuration in vars/*/networking.dhall

Then run `make` to update the yaml files.

To manage the configuration, use an existing group such as `monitoring` and/or add an entry in the `playbooks/site.yaml`

Use git-review to submit the change and let the CI create and run the playbook.
