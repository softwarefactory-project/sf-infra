# Sync images role

## Main goal

Main goal of that role is to synchronize CRC extracted image from
upstream OpenStack cloud provider to other OpenStack cluster.
This role also gives possibility to share that image between other
projects on the same OpenStack cluster. It is also possible to not share
image between other projects.

## Example vars

```yaml
 upstream_cloud_name: upstreamOS-project
 remote_cloud_names:
   - cloud_name: downstreamOS-project-parent
     child_cloud_names:
       - name: downstreamOS-project-child
         project_id: someuuid
       - name: my-cloud-2
         project_id: someuuid2
   - cloud_name: test
     child_cloud_names: []
```

Where:

```sh
 upstream_cloud_name => a name of the cloud resource from ~/.config/openstack/clouds.yaml
                        that is poiting to a upstream OpenStack cloud, that
                        has the image, that later will be pulled by remote_cloud_names
                        and share between {{ remote_cloud_names.[*].child_cloud_names }}
 remote_cloud_names:
 - cloud_name        => (parent) name of the cloud resource from ~/.config/openstack/clouds.yaml
   child_cloud_names => dict, with:
        name => name of the cloud resources from ~/.config/openstack/clouds.yaml
                that parent will share an image to. NOTE: projects needs to be in the same cluster!
        project_id => project_id on which the child_cloud_names.name belongs to.
                      It is required to be more sure, that the image is shared
                      to that project. You can take the project_id by doing:
                      - `openstack project list | grep your-project`
                      or
                      - openstack server list -c ID -f value | head -n1 | xargs openstack server show -c project_id -f value
                      or
                      - openstack --debug server list | grep project_id
```

To get `project_id`, do:

```sh
export OS_CLOUD=cloud_name
openstack server list -c ID -f value | head -n1 | xargs openstack server show -c project_id -f value
```

Example playbook execution:

```sh
ansible-playbook \
  -e "upstream_cloud_name=nodepool-tripleo" \
  -e "{'remote_cloud_names': [{'cloud_name':'tripleo-ci', 'child_cloud_names': ['my-cloud','my-cloud-2']}, {'cloud_name':'test', 'child_cloud_names': []}]}" \
  -e "sync_extracted_qcow2_dir=/home/centos/extracted-crc" \
  ./playbooks/crc/sync-crc-images.yaml"
```
