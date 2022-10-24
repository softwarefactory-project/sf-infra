# Add ibm triple-standalone cloud

The deployment is done in few steps:

1. install the cloud on a bm instance and deploy needed instances

## Deploy triple-standalone cloud and instances

- gen a password for the cloud and create the zuul secret from sf-infra directory:

`
$ export cloud=ibm-bm3-nodepool
$ uuidgen > /tmp/$cloud
$ zuul-client --zuul-url https://softwarefactory-project.io/zuul encrypt \
    --project software-factory/sf-infra --tenant  local
    --secret-name $cloud --field password
    --infile /tmp/$cloud  >> zuul.d/secrets.yaml
`

- add in the *in* statement the function to generate only the configuration for baremetal03 instance:

`
in    mk_cloud baremetal02
    # [ mk_baremetal "baremetal03.rdoproject.org" "169.60.49.226" ]
`

- update the configuration:

`
podman run --rm -it --volume $PWD:/workspace/sf-infra/:Z --volume ~/.cache:/workspace/.cache:Z quay.io/software-factory/zuul-worker-dhall /bin/bash -c "cd /workspace/sf-infra && make"
`

- create playbooks/host_vars/baremetal03.rdoproject.org.yaml with the available devices for nova:

`
# shared variables are in playbooks/group_vars/baremetal.yaml
devices:
  - /dev/nvme0n1
  - /dev/nvme1n1
  - /dev/nvme2n1
`

- add deployment data on standalone_deployments field on playbooks/host_vars/bridge.softwarefactory-project.io.yaml

`
ibm_bm3_ip: 169.60.49.236
standalone_deployments:
  - baremetal_name: ibm-bm3
    baremetal_ip: "{{ ibm_bm3_ip }}"
    cloud: ibm-bm3-nodepool
    password: "{{ ibm_bm3_nodepool.password | default('') }}"
    tripleo_repos_branch: wallaby
    namespace: quay.io/tripleowallabycentos8
    servers:
      - name: mirror.regionone.ibm-bm3-nodepool.rdoproject.org
        flavor: afs
      - name: ibm-bm3-nodepool-launcher.softwarefactory-project.io
        flavor: m1.medium
      - name: ibm-bm3-ze.softwarefactory-project.io
        flavor: m1.large
      - name: ibm-bm3-zfgw.softwarefactory-project.io
        flavor: m1.small
`
- add password on zuul/jobs.yaml where pass for others deployments are defined:

`
secrets:
  - name: ibm_bm3_nodepool
    secret: ibm-bm3-nodepool`
`

- commit and propose a review with the content to setup the cloud

## Add zuul and nodepool instances on sf.io

- run sfconfig to deploy the new hosts
...
