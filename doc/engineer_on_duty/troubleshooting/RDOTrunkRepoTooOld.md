# RDOTrunkRepoTooOld

## Why is it firing ?

A RDO Trunk repository has not been updated.

## To check

Get the affected branch from the alert worker name, for example: `worker = centos-rocky`.
Then check the latest change merged on review.opendev.org, for example: https://review.opendev.org/q/branch:stable/rocky+status:merged
If the latest change merged is newer than the latest DLRN build, there is an issue.

## Solution

You need to check the `DLRN` service on the affected builder host.
You can kill the process and run the command `run-dlrn.sh` (example how yo use
it you can also find in cron job).
