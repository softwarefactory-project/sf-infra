bind_mounts:
  - {source: '/mnt/logs', dest: '/var/www/logs', owner: "loguser", group: "apache"}

acme_domains:
  - domain: fedora.softwarefactory-project.io
    sf_gateway: true
