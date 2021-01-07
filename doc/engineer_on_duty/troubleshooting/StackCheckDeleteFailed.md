StackCheckDeleteFailed
======================

Why is it firing ?
------------------
Some Openstack Heat stacks are in `deleted` state or in `failed` state.

To check
--------
- check by using Openstack client: `openstack stack list | grep -i DELETE_FAILED`

Solution
--------
If there are some stacks in `DELETE_FAILED` state, you should check for what
this stacks are created. If there are not required, take this procedure to
remove it:

- openstack stack list
```
--------------------------------------+---------------------------------+--------------------+----------------------+----------------------+
| ID | Stack Name | Stack Status | Creation Time | Updated Time |
+--------------------------------------+---------------------------------+--------------------+----------------------+----------------------+
...
| 9d86e87d-ddde-400b-a125-d91989e4915b | baremetal_30906_1_96525 | CREATE_COMPLETE | 2020-11-13T09:40:44Z | None |
| 4ae5ce73-8cca-4a20-9233-4f25a4f4b9da | baremetal_28118-extra | DELETE_FAILED | 2020-10-21T17:46:37Z | 2020-11-13T13:40:11Z | << focus on this one
| 33dd1104-1c1d-40c8-b54c-9b70c580ffc8 | baremetal_28118 | DELETE_FAILED | 2020-10-21T17:42:02Z | 2020-11-13T13:40:24Z |
...
+--------------------------------------+---------------------------------+--------------------+----------------------+----------------------+
```
- openstack stack resource list 4ae5ce73-8cca-4a20-9233-4f25a4f4b9da
```
+-----------------------------+--------------------------------------+----------------------------+-----------------+----------------------+
| resource_name | physical_resource_id | resource_type | resource_status | updated_time |
+-----------------------------+--------------------------------------+----------------------------+-----------------+----------------------+
| baremetal_networks | 18db8d91-db4d-4427-b1dd-392eaa22d243 | OS::OVB::BaremetalNetworks | CREATE_COMPLETE | 2020-10-21T17:46:42Z |
| openstack_baremetal_servers | 81d714fe-19df-45f9-8bca-1edbfde59718 | OS::Heat::ResourceGroup | DELETE_FAILED | 2020-10-21T17:46:42Z | << focus on this one
+-----------------------------+--------------------------------------+----------------------------+-----------------+----------------------+
```
- openstack stack resource list 81d714fe-19df-45f9-8bca-1edbfde59718
```
+---------------+--------------------------------------+---------------------+-----------------+----------------------+
| resource_name | physical_resource_id | resource_type | resource_status | updated_time |
+---------------+--------------------------------------+---------------------+-----------------+----------------------+
| 0 | 4766ebb6-37ae-485b-a222-c8a3342cdb05 | OS::OVB::ServerPair | DELETE_FAILED | 2020-10-21T17:46:57Z | << focus on this one
+---------------+--------------------------------------+---------------------+-----------------+----------------------+
```
- openstack stack resource list 4766ebb6-37ae-485b-a222-c8a3342cdb05
```
+------------------+--------------------------------------+-------------------------+--------------------+----------------------+
| resource_name | physical_resource_id | resource_type | resource_status | updated_time |
+------------------+--------------------------------------+-------------------------+--------------------+----------------------+
| baremetal_ports | 79caa265-984a-4463-b82f-a3505b5302df | OS::OVB::BaremetalPorts | CREATE_COMPLETE | 2020-10-21T17:47:14Z |
| baremetal_server | ead643e1-f176-45eb-b501-71c0f4ffdca6 | OS::Nova::Server | DELETE_IN_PROGRESS | 2020-10-21T17:47:14Z | << focus on this one
+------------------+--------------------------------------+-------------------------+--------------------+----------------------+
```
- openstack server show ead643e1-f176-45eb-b501-71c0f4ffdca6
```
+-----------------------------+--------------------------------------------------------------------------+
| Field | Value |
+-----------------------------+--------------------------------------------------------------------------+
| OS-DCF:diskConfig | MANUAL |
| OS-EXT-AZ:availability_zone | nova |
| OS-EXT-STS:power_state | Running |
| OS-EXT-STS:task_state | deleting | <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< HERE SHOULD NOT BE deleting
| OS-EXT-STS:vm_state | active |
| OS-SRV-USG:launched_at | 2020-10-21T17:51:55.000000 |
| OS-SRV-USG:terminated_at | None |
| accessIPv4 | |
| accessIPv6 | |
| addresses | |
| config_drive | |
| created | 2020-10-21T17:50:48Z |
| flavor | ci.m1.small (59ea8095-37b7-4511-8ec5-8a6d48820b15) |
| hostId | 12345 |
| id | ead643e1-f176-45eb-b501-71c0f4ffdca6 |
| image | CentOS-8-x86_64-GenericCloud-1911 (0c943b45-0744-4af6-a7f6-fda97355d9af) |
| key_name | extranode-key-28118 |
| name | baremetal-28118-extra_0 |
| progress | 0 |
| project_id | 123456 |
| properties | |
| status | ACTIVE | <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< HERE SHOULD NOT BE ACTIVE STATE
| updated | 2020-11-13T14:06:41Z |
| user_id | 123545 |
| volumes_attached | |
+-----------------------------+--------------------------------------------------------------------------+
```
- openstack server set ead643e1-f176-45eb-b501-71c0f4ffdca6 --state=active
- openstack server show ead643e1-f176-45eb-b501-71c0f4ffdca6
```
# the state should be changed
+-----------------------------+--------------------------------------------------------------------------+
| Field | Value |
+-----------------------------+--------------------------------------------------------------------------+
| OS-DCF:diskConfig | MANUAL |
| OS-EXT-AZ:availability_zone | nova |
| OS-EXT-STS:power_state | Running |
| OS-EXT-STS:vm_state | active | <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< this state is OK for us
| OS-EXT-STS:vm_state | active |
| OS-SRV-USG:launched_at | 2020-10-21T17:51:55.000000 |
| OS-SRV-USG:terminated_at | None |
| accessIPv4 | |
| accessIPv6 | |
| addresses | |
| config_drive | |
| created | 2020-10-21T17:50:48Z |
| flavor | ci.m1.small (59ea8095-37b7-4511-8ec5-8a6d48820b15) |
| hostId | 12345 |
| id | ead643e1-f176-45eb-b501-71c0f4ffdca6 |
| image | CentOS-8-x86_64-GenericCloud-1911 (0c943b45-0744-4af6-a7f6-fda97355d9af) |
| key_name | extranode-key-28118 |
| name | baremetal-28118-extra_0 |
| progress | 0 |
| project_id | 123456 |
| properties | |
| status | ACTIVE |
| updated | 2020-11-13T14:09:04Z |
| user_id | 123545 |
| volumes_attached | |
+-----------------------------+--------------------------------------------------------------------------+
```

Finally delete the stack:
- openstack stack delete 4ae5ce73-8cca-4a20-9233-4f25a4f4b9da -y

If the stack is still there, check stack status and delete it once again:
- openstack stack check 4ae5ce73-8cca-4a20-9233-4f25a4f4b9da
- openstack stack list --nested | grep 4ae5ce73-8cca-4a20-9233-4f25a4f4b9da | awk '{print $2}' | xargs openstack stack delete -y

NOTE:
If you are sure, that the stacks should be deleted, you can just type:
```
for stack in $(openstack --os-cloud "${OS_CLOUD}" stack list | grep "DELETE_FAILED" | awk '{print $2}'); do
    openstack  --os-cloud "${OS_CLOUD}" stack check "${stack}"
    openstack --os-cloud "${OS_CLOUD}" stack list --nested \
        | grep "${stack}" \
        | awk '{print $2}' \
        | xargs openstack --os-cloud "${OS_CLOUD}" stack delete -y"
done
```
