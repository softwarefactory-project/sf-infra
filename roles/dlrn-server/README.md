Role to set up a DLRN server using puppet-dlrn
==============================================

This role will prepare a custom configuration for puppet-dlrn on a server,
then run a manifest to deploy DLRN using the required parameters.

# Variables

The role will use the following variables, defined in the inventory:

* `dlrn_server_type`, which can be set to "primary" or "backup".
* `dlrn_enable_https`, set to `true` or `false`.
* `dlrn_host` is a unique identifier for the DLRN host being deployed.
  Since puppet-dlrn requires a Hiera file to fetch its parameters from,
  the role supports multiple DLRN host profiles, which will be defined
  by by the `dlrn_host` parameter. The role will look for a file named
  `<dlrn_host>-common.yaml` in the `templates` directory, and use it for
  the Hiera file passed to Puppet. A sample test-common.yaml file is
  provided.