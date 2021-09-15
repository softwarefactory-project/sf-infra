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
curl -k -sSL -X "GET" -H "Authorization: Bearer $TOKEN" -H "Content-Type: application/json" "https://localhost:8443/api/v1/repository/$REPOSITORY" | jq

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

###
