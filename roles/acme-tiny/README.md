# acme-tiny : manage let's encrypt certificate

## Variables

- `acme_challenges_dir`: The directory to store challenges
  :default: /var/www/challenges

- `acme_keys_dir`: The directory to store keys
  :default: /var/lib/software-factory/bootstrap-data/acme-tiny

- `acme_certs_dir`: The directory to store certs
  :default: /etc/letsencrypt/pem

- `acme_vhost_owner`: The name of the vhost owner
  :default: root

- `acme_domains`: The list of domains
  :default: []


## Contribute

This role is entirely defined in the `role.dhall` file.
Update the content by running these commands:

  dhall to-directory-tree --output templates <<< "(./role.dhall).Templates"
  dhall-to-yaml --output tasks/main.yaml     <<< "(./role.dhall).Tasks"
  dhall-to-yaml --output defaults/main.yaml  <<< "(./role.dhall).Defaults"
  dhall text > README.md                     <<< "(./role.dhall).README"
