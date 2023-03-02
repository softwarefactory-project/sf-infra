Deploy Microshift service
=========================

Example deployment
------------------

* Install Ansible:

```sh
sudo dnf install -y ansible-core
ansible-galaxy collection install community.general
```

* Create ansible config:

```sh
cat << EOF > ansible.cfg
[defaults]
roles_path = roles/
command_warnings = True
force_handlers = True

[ssh_connection]
pipelining = True
EOF
```

* Create inventory:

```sh
cat << EOF > inventory.yaml
all:
  vars:
    openshift_pull_secret: |
      < HERE IS pull.secret.txt content >
  hosts:
    microshift.dev:
      ansible_port: 22
      ansible_host: 127.0.0.1
      ansible_user: centos
EOF
```

* Create playbook:

```sh
cat << EOF > deploy-microshift.yaml
---
- hosts: microshift.dev
  vars:
    fqdn: microshift.dev
    use_copr_microshift: false
  roles:
    - extra/microshift
EOF
```

* Deploy Microshift:

```sh
ansible-playbook -i inventory.yaml deploy-microshift.yml
```
