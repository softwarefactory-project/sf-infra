Role to set up a custom SSH port
================================

This role will configure a custom SSH port for your host, falling back to
the default port 22 for the initial connection if needed.

The role will take care of:

* Setting the custom port in `/etc/ssh/sshd_config`
* Adjusting the SELinux configuration to allow the custom port to be used.
* Create a firewalld rule to open the new port, if firewalld is installed.

It is based on this [blog post by dmsimard](https://dmsimard.com/2016/03/15/changing-the-ssh-port-with-ansible/), with some adjustments.

# Usage

* First, you need to set the desired `ansible_port` value in the inventory:

```yaml
---
all:
  hosts:
    testhost:
      ansible_host: 1.2.3.4
      ansible_port: 3300
```

* Then, create the playbook using this role. Make sure to specify
  `gather_facts: no` when including the role, otherwise the first execution
  will fail to use the custom port.

```yaml
---
- hosts: testhost
  gather_facts: false
  roles:
    - custom-ssh-port
```
