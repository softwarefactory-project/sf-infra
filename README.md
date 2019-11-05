Operational Playbook for softwarefactory-project.io
===================================================

There are a few jobs:

* sf-infra-create-bridge creates the bridge, and it is the only one running inside a container
* sf-infra-configure-tenants setup openstack tenant (running from the bridge)
* sf-infra-create-hosts creates new host and display their IP
* sf-infra-configure-hosts run the site.yaml playbook

The idea is to have openstacksdk tasks in jobs that are running only when needed.
Then most of the work is done with the configure-hosts job that use a static inventory.

# To create a new hosts (2 changes):

* Add it to the right playbooks/vars/ file, e.g. playbooks/vars/infra-sf.yaml servers list
* Commit, git-review and wait for the gate job to finish to collect it's public ip
* Add the server to the inventory (ansible/hosts.yaml) and to the site.yaml
* Commit and git-review
