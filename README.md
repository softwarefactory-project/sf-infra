Operational Playbook for softwarefactory-project.io
===================================================

# Inventory

This project manages:

* mirror.regionone.vexxhost.rdoproject.org
* centos8-rpm-packaging-ci.rdoproject.org
* rpm-packaging-ci.rdoproject.org
* registry.rdoproject.org
* trunk-centos8.rdoproject.org
* trunk-centos7.rdoproject.org
* managesf.review.rdoproject.org
* elk.rdoproject.org
* logserver.rdoproject.org
* images.rdoproject.org
* www.rdoproject.org
* dlrn-db.rdoproject.org
* backup.rdoproject.org
* trunk.rdoproject.org
* mirror.regionone.rdo-cloud.rdoproject.org
* rdo-ci-cloudslave01.ci.centos.org
* rdo-ci-cloudslave02.ci.centos.org
* rdo-ci-cloudslave03.ci.centos.org
* rdo-ci-cloudslave04.ci.centos.org
* rdo-ci-cloudslave05.ci.centos.org
* bridge.softwarefactory-project.io
* logreduce-mqtt-01.softwarefactory-project.io
* prometheus.monitoring.softwarefactory-project.io
* ara.softwarefactory-project.io
* redhat-oss-git-stats.softwarefactory-project.io
* elk.softwarefactory-project.io
* managesf.softwarefactory-project.io
* nodepool-builder.softwarefactory-project.io
* k1s02.softwarefactory-project.io
* zs.softwarefactory-project.io
* koji.softwarefactory-project.io
* integrations.softwarefactory-project.io
* fedora.softwarefactory-project.io
* ovirt.softwarefactory-project.io
* ovirt-staging.softwarefactory-project.io
* ansible.softwarefactory-project.io
* ze01.softwarefactory-project.io
* ze02.softwarefactory-project.io
* ze03.softwarefactory-project.io
* ze04.softwarefactory-project.io
* ze05.softwarefactory-project.io
* ze06.softwarefactory-project.io
* ze07.softwarefactory-project.io
* zm01.softwarefactory-project.io
* zm02.softwarefactory-project.io
* zm03.softwarefactory-project.io
* zm04.softwarefactory-project.io
* zm05.softwarefactory-project.io
* zm06.softwarefactory-project.io
* zm07.softwarefactory-project.io
* zm08.softwarefactory-project.io

# Managing Groups

Groups are hardcoded in the `conf/types/Group.dhall` file. Extra groups
needed outside of the sf-infra project needs to be added to this file.

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
