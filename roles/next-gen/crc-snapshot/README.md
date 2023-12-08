# CRC snapshot role

## Main goal

The main goal of that role is to create a script that will trigger
Ansible playbook that will recreate the CRC image base on
`cloud_names` Ansible variable (which is a list).

## Example variable output

```yaml
cloud_names:
  - crc_cloud_name: nodepool-tripleo
    crc_flavor_name: ci.m1.xlarge
    crc_ssh_keypair_name: image-builder
    crc_net_id: 7abff1a9-a103-46d0-979a-1f1e599f4f41
    crc_ssh_pub_path: ~/.ssh/id_ed25519.pub
    crc_system_distro: centos
    crc_system_release: 9
    crc_version: '2.19'
    crc_sync_extracted_qcow2_dir: ~/extracted-crc
    crc_ansible_log_dir: /var/log/crc-snapshot
    crc_nested_crc: true
    crc_nested_final_image_prefix: centos-9-crc-2.19-latest
    crc_nested_normal_image_prefix: centos-9-crc-2.19-[0-9-]{16}
    crc_extracted_crc: true
    crc_extracted_final_image_prefix: coreos-crc-extracted-2.19-latest
    crc_extracted_normal_image_prefix: coreos-crc-extracted-2.19-[0-9-]{16}

  - crc_cloud_name: tripleo-ci
    crc_flavor_name: ci.standard.xl
    crc_ssh_keypair_name: image-builder
    crc_net_id: eceac180-5a4d-4b1d-b916-1d4e8f19b873
    crc_ssh_pub_path: ~/.ssh/id_ed25519.pub
    crc_system_distro: rhel
    crc_system_release: 9
    crc_version: '2.19'
    crc_ansible_log_dir: /var/log/crc-snapshot
    crc_nested_crc: true
    crc_nested_final_image_prefix: rhel-9-crc-latest
    crc_nested_normal_image_prefix: rhel-9-crc-[0-9-]{16}
    crc_extracted_crc: false

  - crc_cloud_name: nodepool-tripleo
    crc_flavor_name: ci.m1.xlarge
    crc_ssh_keypair_name: image-builder
    crc_net_id: 7abff1a9-a103-46d0-979a-1f1e599f4f41
    crc_ssh_pub_path: ~/.ssh/id_ed25519.pub
    crc_system_distro: centos
    crc_system_release: 9
    crc_version: '2.29'
    crc_sync_extracted_qcow2_dir: ~/extracted-crc
    crc_ansible_log_dir: /var/log/crc-snapshot
    crc_nested_crc: false
    crc_nested_final_image_prefix: centos-9-crc-2.29-latest
    crc_nested_normal_image_prefix: centos-9-crc-2.29-[0-9-]{16}
    crc_extracted_crc: true
    crc_extracted_final_image_prefix: coreos-crc-extracted-2.29-latest
    crc_extracted_normal_image_prefix: coreos-crc-extracted-2.29-[0-9-]{16}
```

Where:

```sh
  crc_cloud_name                    => cloud name defined in ~/.config/openstack/clouds.yaml
  crc_flavor_name                   => flavor to spawn VM where later CRC will be deployed
  crc_ssh_keypair_name              => ssh keypair to connect from your host to the VM
  crc_net_id                        => network id of new VM
  crc_ssh_pub_path                  => custom defined SSH public key by injecting it via user data
  crc_system_distro                 => system distribution to create a VM
  crc_system_release                => release of the system distribution
  crc_version                       => what CRC version should be deployed
  crc_sync_extracted_qcow2_dir      => where the crc.qcow2 image should be pulled on your host
  crc_ansible_log_dir               => where Ansible should store logs
  crc_nested_crc                    => should it create a nested CRC image?
  crc_nested_final_image_prefix     => what name should finally have the nested CRC image
  crc_nested_normal_image_prefix    => regex of the old nested CRC images. Will be used in future promotion
  crc_extracted_crc                 => should it create an extracted CRC image?
  crc_extracted_final_image_prefix  => what name should finally have the extracted CRC image
  crc_extracted_normal_image_prefix => regex of the old extracted CRC image. Will be used in future promotion
```
