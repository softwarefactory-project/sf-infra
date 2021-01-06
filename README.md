# Software-Factory and RDO infra management

## Why

The project is used to manage softwarefactory-project.io and
rdoproject.org infrastructures using CI/CD workflow.

Zuul is used to launch jobs to create, configure and update the
infrastructure on OpenStack public clouds.

External instances could also be managed by the project.

Monitoring is configured for all the hosts define on the project using
prometheus, the monitoring is located at
https://prometheus.monitoring.softwarefactory-project.io

## How

The infrastucture deployment and configuration is done by running Zuul
jobs defined in ./zuul.d/jobs.yaml. The Zuul jobs create, configure and
update the infrastructure:

* Dhall is used to generate ansible inventory and variables files needed to
  manage the infrastructure.
* OpenStack networks, subnets, routers, security groups and instances are
  managed using ansible os_* modules. Roles in ./roles are used to configure
  hosts and services.


#### The main directories are:

* ansible: contains ansible inventory and configuration files.
* Infra: Dhall types to manage the infrastructure.
* monitoring: prometheus configuration and rules.
* playbooks: main playbooks used to manage the infrastructure.
* roles: ansible roles.
* tests: playbooks used with molecule to validate roles.
* vars: configuration directory for the infrastructure.
* zuul.d: zuul jobs location.


#### Vars directory

From a user perspective, the most important files and directories in vars
directory are:

```
vars
├── common.dhall
├── files
├── infra-rdo
├── infra-sf
├── nodepool-rdo
├── nodepool-sf
├── nodepool-tripleo
```

This directory contains the configuration for OpenStack tenants, there is one
directory per project:
* infra-sf and infra-rdo contains configuration to manage network and servers
  configuration for both projects.
* nodepool-* contains configuration to manage network configuration for nodepool
  projects.
* common.dhall for data shared in tenants (images, flavors, ...).
* files directory contains static files like ssh pubkey.


#### Playbooks directory

These playbooks are executed if needed (configure-tenants.yaml is only
executed if networks or servers should be created or updated). Here a
description of the most importants:

##### create-bridge.yaml

This playbook is used to manage bridge deployment and configuration. The
bridge is the host where the ansible playbooks are run.

##### configure-tenants.yaml

Tenant management, to creates and configures networks, subnets, routers
and security groups.

##### create-hosts.yaml

Host management, to create and configure servers and volumes.

##### site.yaml

Host configuration, to define roles and playbooks per host or group.

##### rdo-registry.yaml

Specific configuration for rdo-registry deployment.


## Documentation

How-to are located in doc directory:

* how-to-manage-instance.md
* how-to-networking.md
* how-to-dedicated-workspace-for-a-project.md
* how-to-vault-secret.md
* how-to-add-a-ci-project.md

Have a look to doc/dhall-onboarding/README.md to get started with dhall.

## Inventory

All the servers managed by the project could be found on doc/inventory.md
