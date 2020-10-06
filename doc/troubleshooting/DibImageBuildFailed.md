DibImageBuildFailed
===================

Why is it firing ?
------------------

There was an error with a DIB image build.

Solution
--------

* ssh into nodepool-builder
* check last build logs at /var/www/nodepool-logs/xxx.log

Note that logs are also available to share with others via this URL: https://softwarefactory-project.io/nodepool-log/

### Identify which element has a problem

If it belongs to the following list: https://github.com/openstack/diskimage-builder/tree/master/diskimage_builder/elements
then a patch upstream might be needed.

### Is it a memory error?

For example: `Original error from libvirt: internal error: [...] Cannot allocate memory` is the last error in the logs.

* Reboot the nodepool-builder instance
* make sure the nodepool services are up and running
* check the latest build and ensure it succeeds.
