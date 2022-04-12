# Quay service deployment

## Example, how to deploy Quay service
```
- hosts: quay.dev
  vars:
    fqdn: quay.dev
    self_signed_certs: true
    initial_config: false
    quay_users:
      admin:
        email: release@somemail.com
        password: admin
  tasks:
    - name: Setup quay
      include_role:
        name: quay
        tasks_from: main.yml
  roles:
    - hostname
```

Then on the host, you can try login:
```
export GODEBUG=x509ignoreCN=0
podman login quay.dev -u admin -padmin
```

## Example Quay API calls

All requests have been made with exported variables:
```
TOKEN=<SOME TOKEN>
REPOSITORY="admin/test"
TAG="latest"
```

### Get repository informations
```
curl -k -sSL -X "GET" -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" "https://localhost/api/v1/repository/$REPOSITORY" | jq

--- output ---
{
  "namespace": "admin",
  "name": "fedora",
  "kind": "image",
  "description": "",
  "is_public": true,
  "is_organization": false,
  "is_starred": false,
  "status_token": "",
  "trust_enabled": false,
  "tag_expiration_s": 1209600,
  "is_free_account": true,
  "state": "NORMAL",
  "tags": {
    "34": {
      "name": "34",
      "image_id": "e942593795b6de3745b95abd2e426bd9857c48a537e36947ad26e72a346dbea3",
      "size": 71057254,
      "last_modified": "Fri, 17 Sep 2021 12:57:18 -0000",
      "manifest_digest": "sha256:b5290db40008aae9272ad3a6bd8070ef7ecd547c3bef014b894c327960acc582"
    }
  },
  "can_write": false,
  "can_admin": false
}
```

### Registry image pruning

We do not want to keep the uploaded container images forever. To allow
image pruning we will be using [Quay tag expiration](https://docs.projectquay.io/use_quay.html#tag-expiration).

Note that the pruning script will set the expiration date to 1 day after
its execution, and the Time Machine functionality in Quay will allow
recovering the image for some time (two weeks by default). Thus, its effect
in disk usage will not be immediate.

To configure tag expiration in the role, we need to declare the following
variables:

- `quay_enable_prune`: If set to `true`, the pruning script will be enabled.
- `quay_pruner_log_directory`: This is the directory that will store the
  pruning script logs.
- `quay_organizations`: This is a list of all the organizations to be
  analyzed. Each of the items in the list must contain the following:
  - `name`: Name of the organization.
  - `token`: For each organization, a unique OAuth2 token must be created,
    following the instructions from the [Quay API documentation](https://docs.quay.io/api/).
    The token need full permissions over repositories in the organization.
    Since the token can be used for admin operations, it must be kept
    secret, and should never be stored in plain text. Use Ansible Vault
    instead.
  - `prune_days`: This value defines the number of days before a tag
    is expired.
- `quay_pruner_dlrn_endpoints`: This list defines the DLRN API endpoints
  that will be queried to create a list of protected image tags.
- `quay_pruner_extended_keeplist`: This list defines the protected image
  tags. The pruning script will check the DLRN API endpoint for those tags,
  and add the tags and those hashed tags they point to to a keep list, so
  they will not be expired even if they are older than `prune_days`.

This is an example configuration:

```
quay_enable_prune: true

quay_pruner_log_directory: /var/log/quay_tag_pruner

quay_organizations:
    - name: tripleomaster
      token: 'testtoken'
      prune_days: 7

quay_pruner_dlrn_endpoints:
  - api-centos9-master-uc
  - api-centos8-master-uc
  - api-centos8-wallaby
  - api-centos8-victoria
  - api-centos8-ussuri
  - api-centos8-train
  - api-centos-train

quay_pruner_extended_keeplist:
  - current-tripleo
  - current-tripleo-rdo
  - current-tripleo-rdo-internal
  - tripleo-ci-testing
  - promoted-components
  - component-ci-testing
  - previous-current-tripleo
```
