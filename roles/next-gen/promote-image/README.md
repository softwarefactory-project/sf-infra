# Promote image role

## Main goal

The purpose of this role is to ensure the latest CRC images (both
nested and extracted) are validated before being used in Zuul CI
jobs. Previously, we used images that included dates in their names,
which required updating the nodepool label each time we switched to a
new image. This process had to be repeated every 29 days due to the CA
certificate expiring in 30 days, creating additional work for the
infrastructure team. To streamline this, we introduced a promotion
system.

To prevent broken images from being deployed to the OpenStack cloud,
we verify the new image by:

* Launching an instance from the latest image
* Running tests
(If the tests pass)
* Renaming the old "latest" image to include a date suffix
* Renaming the new image with the "latest" suffix to set it as the
  current active image

## Example vars

```yaml
cloud_names:
  - crc_cloud_name: some-cloud
    crc_flavor_name: ci.m1.xlarge
    crc_ssh_keypair_name: image-builder
    crc_net_id: 7abff1a9-a103-46d0-979a-1f1e599f4f41
    crc_ssh_pub_path: ~/.ssh/id_ed25519.pub
    crc_nested_crc: true
    crc_nested_final_image_prefix: centos-9-crc-latest
    crc_nested_normal_image_prefix: centos-9-crc-
    crc_extracted_crc: true
    crc_extracted_final_image_prefix: coreos-crc-extracted-latest
    crc_extracted_normal_image_prefix: coreos-crc-extracted-

  - crc_cloud_name: some-cloud-2
    crc_flavor_name: ci.m1.xlarge
    crc_ssh_keypair_name: image-builder
    crc_net_id: 123124123-a103-46d0-979a-123412341234
    crc_ssh_pub_path: ~/.ssh/id_ed25519.pub
    crc_nested_crc: true
    crc_nested_final_image_prefix: rhel-9-crc-latest
    crc_nested_normal_image_prefix: rhel-9-crc-
    crc_extracted_crc: false
```

Where:

```sh
cloud_names => list - name of the Openstack cluster to operate on
    crc_cloud_name                      => name of the cloud resource name available in ~/.config/openstack/clouds.yaml
    crc_flavor_name                     => name of the flavor that will be used for tests
    crc_ssh_keypair_name                => name of the keypair available in {{ crc_cloud_name }}
    crc_net_id                          => id of the network that will be used for spawning VM
    crc_ssh_pub_path                    => path for SSH pub key that should be injected via cloud-init
    crc_nested_crc                      => boolean should the nested image be created?
    crc_nested_final_image_prefix       => name how the image after promotion should look like
    crc_nested_normal_image_prefix      => name of the image prefix without the date. For example:
                                           for image: centos-9-crc-2023-10-13-06-53, this var should
                                           look like: centos-9-crc-
    crc_extracted_crc                   => boolean should extracted image be created?
    crc_extracted_final_image_prefix    => name how the image after promotion should look like
    crc_extracted_normal_image_prefix   => name of the image prefix without the date. For example:
                                           for image: coreos-crc-extracted-2023-10-13-06-53
                                           this var should look like: coreos-crc-extracted-
```

Example playbook execution:

* For extracted:

```sh
ansible-playbook \
  -e "ssh_pub_path=~/.ssh/id_ed25519.pub" \
  -e "final_image_prefix=coreos-crc-extracted-latest" \
  -e "normal_image_prefix=coreos-crc-extracted-" \
  -e "cloud_name=nodepool-tripleo" \
  "$SF_INFRA_REPO_DIR/playbooks/crc/promote-crc-image.yaml"
```

* For nested:

```sh
ansible-playbook \
  -e "ssh_pub_path=~/.ssh/id_ed25519.pub"
  -e "final_image_prefix=centos-9-crc-latest" \
  -e "normal_image_prefix=centos-9-crc-" \
  -e "image_ssh_user=zuul" \
  -e "cloud_name=nodepool-tripleo" \
  "$SF_INFRA_REPO_DIR/playbooks/crc/promote-crc-image.yaml"
```
