RDOTrunkRepoTooOld
==================

Why is it firing ?
------------------
Probably none change was done in upstream repository in Openstack.

To check
--------
Check if some change was made on review.opendev.org, by running
example query: `(branch:stable/victoria OR branch:master)`.
It may be a situation, that RDO Trunk builder freeze. For that, you
need to login to the builder host and check logs from `dlrn-logs` directory.

Solution
--------
If changes were made in review.opendev.org, probably the `DLRN` service
freeze. In that case (at first please make a step from `To check` section),
you can kill the process and run the command `run-dlrn.sh` (example how yo use
it you can also find in cron job).


When to ignore
--------------
In some cases, builders for old releases may not have enough commit activity.
If that is the case, you can safely ignore the alarm.
