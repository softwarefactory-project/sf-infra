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

* restart nodepool-builder service and make sure that it is up and running
* if problems comes after few DIB builds, flush cache: `sync; echo 3 > /proc/sys/vm/drop_caches`
* reboot the nodepool-builder instance if problem still exists after few builds

### Is that old-appliance?

For example, error looks like:

```
"libguestfs supermin, fixed or old-style appliance on LIBGUESTFS_PATH ", "(search path: /tmp/appliance)", "", "If reporting bugs, run virt-customize with debugging enabled and include ", "the complete output:", "", "  virt-customize -v -x [...]"], "stdout": "[   0.0] Examining the guest ...", "stdout_lines": ["[   0.0] Examining the guest ..."]}
```

* remove directory by typing: `rm -rf /tmp/appliance` . This directory content will be downloaded in next DIB build
* restart nodepool-builder service
* reboot host if problem still exists
