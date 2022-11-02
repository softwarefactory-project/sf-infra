# Add ibm triple-standalone cloud

## The deployment is done in few steps:

1. install the cloud on a bm instance

## Variables for this deployment are defined in few places:

### playbooks/host_vars/baremetal*.yaml
contain host variables for roles defined in playbooks/playbooks/site_rdo.yaml, like which hdd should be used

### playbooks/group_vars/baremetal.yaml
contains group variables for roles defined in playbooks/playbooks/site_rdo.yaml
all baremetal hosts are member of the baremetal group, variables used to setup the hosts (clean hdd, create lv, ...)

### playbooks/zuul/host_vars/playbooks/zuul/host_vars/bridge.softwarefactory-project.io.yaml
This file contains standalone_deployments which is a list with variables for all deployments


### playbooks/group_vars/baremetal.yaml
Contains the flavors which needs to be create directly from the baremetal hosts

## Deploy triple-standalone cloud and instances

1. gen a password for the cloud and create the zuul secret from sf-infra directory:

`
$ export cloud=ibm-bm3-nodepool
$ uuidgen > /tmp/$cloud
$ zuul-client --zuul-url https://softwarefactory-project.io/zuul encrypt \
    --project software-factory/sf-infra --tenant  local \
    --secret-name $cloud --field password \
    --infile /tmp/$cloud  >> zuul.d/secrets.yaml
`

2. add in the *in* statement the function to generate only the configuration for baremetal03 instance:

`
in    mk_cloud baremetal02
    # [ mk_baremetal "baremetal03.rdoproject.org" "169.60.49.226" ]
`

3. update the configuration:

`
podman run --rm -it --volume $PWD:/workspace/sf-infra/:Z --volume ~/.cache:/workspace/.cache:Z quay.io/software-factory/zuul-worker-dhall /bin/bash -c "cd /workspace/sf-infra && make"
`

4. create playbooks/host_vars/baremetal03.rdoproject.org.yaml with the available devices for nova:

`
# shared variables are in playbooks/group_vars/baremetal.yaml
devices:
  - /dev/nvme0n1
  - /dev/nvme1n1
  - /dev/nvme2n1
`

5. add ip on playbooks/host_vars/bridge.softwarefactory-project.io.yaml

`
ibm_bm3_ip: 169.60.49.226
`

6. add deployment data on standalone_deployments field on playbooks/zuul/host_vars/bridge.softwarefactory-project.io.yaml

`
standalone_deployments:
  - baremetal_name: ibm-bm3
    baremetal_ip: 169.60.49.226
    cloud: ibm-bm3-nodepool
    password: "{{ ibm_bm3_nodepool.password }}"
    tripleo_repos_branch: wallaby
    namespace: quay.io/tripleowallaby
    name_prefix: openstack-
`

7. add password on zuul/jobs.yaml where passwords for others deployments where they are defined:

`
secrets:
  - name: ibm_bm3_nodepool
    secret: ibm-bm3-nodepool`
`

8. add openstack client config on playbooks/zuul/templates/clouds.yaml.j2

`
ibm-bm3-nodepool:
  auth:
    auth_url: https://169.60.49.226:13000
    password: {{ ibm_bm3_nodepool.password }}
    project_domain_name: Default
    project_name: nodepool
    user_domain_name: Default
    username: nodepool
  cacert: /home/fedora/.config/openstack/ibm-bm3-ca.crt
  identity_api_version: '3'
  region_name: regionOne
  volume_api_version: '3'
`
The crt will be installed by shiftstack/dev-install at the end of the openstack installation

9. commit and propose a review with the content to setup the cloud

If deployment failed, most of the time, baremetal instance should be redeployed and /home/fedora/.ssh/known_hosts host entries should be removed from the bridge before redeploying

## Add zuul and nodepool instances on sf.io

- run sfconfig to deploy the new hosts