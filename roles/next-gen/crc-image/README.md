# Create snapshot of CRC instance

## Workflow

- localhost got file "~/.ansible_crc_vars.yaml" that contains openshift_pull_secret var,
- localhost spawns crc.dev instance
- crc.dev is deploying crc
- after crc is done, coming back to localhost that is executing snapshot with name {{ nested_crc_snapshot_image_name }}
- remove the crc VM

## To setup the venv environment

```sh
python3 -mvenv crc-snapshot-venv
source crc-snapshot-venv/bin/activate
python3 -mpip install ansible ansible-core==2.13.7 openstackclient python-openstackclient
ansible-galaxy collection install  community.general community.crypto ansible.posix openstack.cloud
```

## Run playbook with the following command

```sh
ansible-playbook playbooks/crc/crc-make-snapshot.yaml
```

NOTE: The Centos 8 stream CRC image will be removed soon.

```sh
ansible-playbook -e "ansible_host_key_checking=False" -e "system_distro=centos" -e "system_release=8" playbooks/crc/crc-make-snapshot.yaml
```

NOTE: The snapshot is using image: cloud-centos-9-stream-tripleo that is using
"zuul" as regular user.

More [info](https://softwarefactory-project.io/r/c/software-factory/sf-infra/+/28356/comments/b4d1830e_5060e846)

### Additional variables

Openstack - preparations

- define image_name
- define image_ssh_user

CRC

- crc_parameters: "--memory 14336 --disk-size 60 --cpus 6"
- nested_virtualization
- disable_selinux

NOTE: Playbook requires: ansible-galaxy collection install openstack.cloud.
If it's not updated, it can raise an error:

```sh
openstack.cloud.server error 'volumes is not found. openstack.compute.v2.server.Server objects do not support setting arbitrary keys through the dict interface.
```
