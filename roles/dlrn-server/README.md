Role to set up a DLRN server using ansible-role-dlrn
====================================================

This role will prepare a custom configuration to use
[ansible-role-dlrn](https://github.com/rdo-infra/ansible-role-dlrn), deploy DLRN
using the required parameters, and perform some additional housekeeping tasks
that do not belong in the generic role.

# Variables

The role will use the following variables, defined in the inventory:

* `dlrn_server_type`, which can be set to "primary" or "backup".
* `dlrn_enable_https`, set to `true` or `false`.
* `dlrn_sshd_port`, an extra SSH port to use by DLRN.
* `dlrn_override_files` allows us to override some files from the
  ansible-role-dlrn checkout, and will be applied *after* the dlrn role
  is executed. You need to specify the dictionary in the following format:

        dlrn_override_files:
            /path/to/overriden_file:
            source: local_override_file

The rest of the variables are the ones specified in the
[README.md file](https://github.com/rdo-infra/ansible-role-dlrn/blob/master/README.md) for
the ansible-role-dlrn role.
