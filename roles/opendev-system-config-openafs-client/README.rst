An ansible role to configure an OpenAFS client

.. note:: This role uses system packages where available, but for
          platforms or architectures where they are not available will
          require external builds.  Defaults will pick external
          packages from OpenStack Infra builds, but you should
          evaluate if this is suitable for your environment.

This role configures the host to be an `OpenAFS
<https://www.openafs.org>`__ client.  Because OpenAFS is very reliant
on distribution internals, kernel versions and host architecture this
role has limited platform support.  Currently supported are

* Debian family with system packages available
* Ubuntu LTS family with external 1.8 series packages
* CentOS 7 and 8 with external packages

**Role Variables**

.. zuul:rolevar:: openafs_client_cell
   :default: openstack.org

   The default cell.

.. zuul:rolevar:: openafs_client_cache_size
   :default: 500000

   The OpenAFS client cache size, in kilobytes.

.. zuul:rolevar:: openafs_client_cache_directory
   :default: /var/cache/openafs

   The directory to store the OpenAFS cache files.

.. zuul:rolevar:: openafs_client_yum_repo_url
   :default: ``https://tarballs.openstack.org/project-config/package-afs-centos7``

   The URL to a yum/dnf repository with the OpenAFS client RPMs.
   These are assumed to be created from the ``.spec`` file included in
   the OpenAFS distribution.

.. zuul:rolevar:: openafs_client_yum_repo_gpg_check
   :default: no

   Enable or disable gpg checking for ``openafs_yum_repo_url``

This role has been copied over from the OpenDev system-config role available
at https://opendev.org/opendev/system-config/src/branch/master/roles/openafs-client,
so it can be modified to be compatible with the CentOS infrastructure we use.
