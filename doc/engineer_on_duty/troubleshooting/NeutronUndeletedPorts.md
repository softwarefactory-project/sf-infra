NeutronUndeletedPorts
=====================

Why is it firing ?
------------------
Probably some ports are in `Down` state and there are older
than delta time (which is set to 3 days).

To check
--------
- it can be related partially to `StackCheckDeleteFailed` alert
- by using openstack client, check ports in down state: `opentack port list | grep -i down`
- take one of described ports and check `updated_at` label
