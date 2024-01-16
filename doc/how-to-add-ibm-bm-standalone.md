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

2. add the clouds in vars/infra-rdo

- each cloud use a dedicated subnet 192.168.25.0/24, 192.168.26.0/24 ...

- create a file to define variables for the cloud vars/infra-rdo/baremetals/baremetal03.dhall

`
let baremetal = { name = "baremetal03.rdoproject.org", ip = "169.60.49.226" }

let instances = ./mkInstances.dhall baremetal.name "ibm-bm3" "192.168.25"

in  { baremetal, instances }
`

- add it on the *in* statement in vars/infra-rdo/ibm-baremetal.dhall:
`
in    [ mkBaremetal (./baremetals/baremetal03.dhall).baremetal ]
    # mkInstances (./baremetals/baremetal03.dhall).instances
    # [ mkBaremetal (./baremetals/baremetal04.dhall).baremetal ]
    # mkInstances (./baremetals/baremetal04.dhall).instances
`

Also add zookeeper security rule in vars/infra-sf/networking.dhall

`
        , { name = "zookeeper"
          , rules =
            [ Infra.Rule::{
              , port = +2281
              , remote_ip_prefix = Some "{{ ibm_bm2_ip }}/32"
              }
            , Infra.Rule::{
              , port = +2281
              , remote_ip_prefix = Some "{{ ibm_bm3_ip }}/32"
              }
            ]
          }
`

Finally, add a rule for statsd in vars/infra-sf/rules/prometheus-statsd.dhall for the host

`
let ibm-bm3 = "169.60.49.226"

in
...
    # Rule.integerMap (Rule.createUdpHost ibm-bm3) [ udp-multiplexer ]

3. update the configuration:

`
$ make
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
    servers:
      - name: mirror.regionone.ibm-bm3-nodepool.rdoproject.org
        flavor: afs
        ip: 192.168.25.10
      - name: ibm-bm3-nodepool-launcher.softwarefactory-project.io
        flavor: m1.medium
        ip: 192.168.25.11
      - name: ibm-bm3-ze.softwarefactory-project.io
        flavor: m1.large
        ip: 192.168.25.12
      - name: ibm-bm3-zfgw.softwarefactory-project.io
        flavor: m1.small
        ip: 192.168.25.13
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

9. Add Proxy jump for each ip (needed by ansible) on vars/directory-tree.dhall

`
Host 192.168.25.11 192.168.25.12 192.168.25.13
    ProxyJump baremetal03.rdoproject.org
`

10. Add squid service for shiftstack team if needed on ./playbooks/zuul/configure-private-clouds.yaml

`
    - name: Configure squid service
      ansible.builtin.include_role:
        name: rdo/ibm-shiftstack-squid
`

11. commit and propose a review with the content to setup the cloud

If deployment failed, most of the time, baremetal instance should be redeployed and /home/fedora/.ssh/known_hosts host entries should be removed from the bridge before redeploying

The first deployment on gate should failed on site_{sf,rdo} for instances need to be created. A recheck on gate will be needed.

## Add data in sf-infra to reach instances from managesf


1. Update arch in roles/infra/install-server/files/arch-managesf.softwarefactory-project.io.yaml

`
- name: ibm-bm3-nodepool-launcher
  private: true
  use_public_ips: yes
  ip: 192.168.25.11
  roles:
  - nodepool-launcher

- name: ibm-bm3-ze
  ip: 192.168.25.12
  private: true
  use_public_ips: yes
  roles:
  - zuul-executor

- name: ibm-bm3-zfgw
  ip: 192.168.25.13
  private: true
  use_public_ips: yes
  roles:
  - zuul-fingergw
`

2. Add the new cert in playbooks/group_vars/ibm-baremetal-nodepool.yaml

The cert can be found on the bridge in /home/fedora/.config/openstack/ibm-bm3-ca.crt

`
self_signed_certs:
  - ca_cert_name: ibm-bm3-nodepool.crt
    ca_cert: |
      -----BEGIN CERTIFICATE-----
      MIIE+DCCAuCgAwIBAgIUen0QaSPoLX02oPzYnn1ATlJN5dMwDQYJKoZIhvcNAQEL
      BQAwEzERMA8GA1UEAwwIc2ltcGxlY2EwHhcNMjIxMTAxMjEzNDQ5WhcNMzIxMDI5
      ...
`
3. Add ssh config on roles/infra/ssh/tasks/main.yaml

`
Host baremetal03.rdoproject.org
    User root
    Port 22
    Hostname 169.60.49.226

Host 192.168.25.11 192.168.25.12 192.168.25.13
    ProxyJump baremetal03.rdoproject.org

Host ibm-bm3-nodepool-launcher*
    ProxyJump baremetal03.rdoproject.org
    Hostname 192.168.25.11

Host ibm-bm3-ze*
    ProxyJump baremetal03.rdoproject.org
    Hostname 192.168.25.12

Host ibm-bm3-zfgw*
    ProxyJump baremetal03.rdoproject.org
    Hostname 192.168.25.13
`

4. commit and propose a review with the content to setup the cloud

## Add zuul and nodepool instances on sf.io

1. Add credential to connect to the cloud on /etc/software-factory/clouds.yaml

   The cert is no needed on sf for it was added previously on sf-infra

   `
   ibm-bm3-nodepool:
     auth:
       auth_url: https://169.60.49.226:13000
       password: $nodepool_password
       project_domain_name: Default
       project_name: nodepool
       user_domain_name: Default
       username: nodepool
     identity_api_version: '3'
     region_name: regionOne
     volume_api_version: '3'
   `
2. Add the zuul executor and fingergw zone in /etc/software-factory/custom-vars.yaml

   `
   zuul_executor_zone:
     - hostname: ibm-bm4-ze
       zone: ibm-bm4-nodepool
   zuul_fingergw_zone:
     - hostname: ibm-bm4-zfgw
       zone: ibm-bm4-nodepool
       gateway_hostname: 169.60.49.233
   `
3. run sfconfig to deploy the new hosts

4.
## Add nodepool configuration

1. on the config repo, create a new provider

`
cp nodepool/providers/ibm-bm2-nodepool.dhall nodepool/providers/ibm-bm3-nodepool.dhall
`
update variables on nodepool/providers/ibm-bm3-nodepool.dhall for:

`
let cloud = "ibm-bm3-nodepool"
...

[ { mapKey = "executor-zone", mapValue = "ibm-bm3-nodepool" } ]
`
2. Add the provider in nodepool/static_config/nodepool-builder.dhall

3. create static config file for nodepool-launcher service

`
cp nodepool/static_config/ibm-nodepool-launcher.dhall nodepool/static_config/ibm-bm3-nodepool-launcher.dhall
`

update variables nodepool/static_config/ibm-bm3-nodepool-launcher.dhall for:

`
let providers = [ Provider.openstack ../providers/ibm-bm3-nodepool.dhall ]
`


4. Add the nodepool/static_config/ibm-bm3-nodepool-launcher.yaml in MANAGED on nodepool/Makefile

5. update the configuration and commit:

`
make
`
