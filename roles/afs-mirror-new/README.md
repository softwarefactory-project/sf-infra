afs-mirror role
===============


# Overview
This is aimed at setting up an AFS mirror for the RDO infrastructure,
based on the same configuration used by the OpenDev Infra mirrors.

# Variables

The role will use the following variables, defined in the inventory:

* `afs_mirror_fqdn` (required) specifies the fully qualified domain name
  used by the mirror httpd configuration.
* `afs_client_cache_size` (optional) specifies the maximum amount of disk
  storage to be used as AFS client cache. Defaults to 50000000 (50 GB).
* `afs_ssl_cert`(required) points to the SSL certificate file used for SSL.
* `afs_ssl_private_key`(required) points to the SSL certificate private key
  used for SSL.
* `afs_ssl_cacert`(required) points to the SSL full trust chain certificate,
  used for SSL. If not available, it can point to the same file as `afs_ssl_cert`.

# Migration notes

If you are migrating a server deployed using the Puppet modules, please take
into account the following details:

* The `httpd` service will likely fail to restart on the first deployment.
  If that is the case, please remove the old 50-<mirror_fqdn>.conf file
  located under /etc/httpd/conf.d and restart the httpd service.
* When applying the role to a server where the Puppet modules where run,
  httpd may not listen on port 443. This happens because the apache Puppet
  module removed all files under /etc/httpd/conf.d before creating its own
  configuration. To fix this, run `yum reinstall mod_ssl` and restart the
  httpd service.

# Limitations

The role has only been tested on CentOS 7 and 8.

The role requires some roles forked from the [OpenDev system-config repo](https://opendev.org/opendev/system-config). These roles include [series of patches](https://review.opendev.org/#/q/status:open+project:opendev/system-config+branch:master+topic:mirror-centos) to add
support for CentOS.
