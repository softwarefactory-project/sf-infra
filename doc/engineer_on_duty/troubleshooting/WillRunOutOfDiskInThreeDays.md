WillRunOutOfDiskInThreeDays
===========================

Why is it firing ?
------------------

Given the current rate of disk usage, all disk space will be used in three days.
The rate is linearly interpolated and therefore might not be accurately representing
the actual data consumption rate.

Solution
--------

### rdo-ci-cloudslave*.ci.centos.org

Usually disk space is being used by .tox leftovers.

* ssh as fedora
* run as root: `find /home/jenkins/workspace/ -maxdepth 1 -type d -mtime +1 -exec rm -rf {} \;`

If in doubt, check the crontab for the cleanup command.
