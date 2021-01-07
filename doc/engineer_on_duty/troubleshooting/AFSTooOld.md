AFSTooOld
=========

Why is it firing ?
------------------
Probably our AFS mirror is outdated with Opendev AFS mirror.
In most cases, problem is on Opendev side.
To ensure that, you should check:

- Grafana AFS mirror: https://grafana.opendev.org/d/HxrNXt2Gk/afs?orgId=1
- Last directories and files in: http://mirror.dfw.rax.opendev.org/
- Last directories and files in: http://mirror.regionone.vexxhost-nodepool-sf.rdoproject.org/

Solution
--------
If in the Grafana dashboard, `mirror.fedora` entry or `mirror.epel` or `mirror.centos`
is older than 3 days, you should ask on `irc` on `#opendev` channel, if
there are some issues with the AFS mirror.

If problem is on our side, you should execute on AFS mirror host:
* check status of openafs client: `systemctl status openafs-client`
* restart `openafs-client` service if necessary
* restart Apache2 service (if neccessary) and check logs
* rerun script for checking date: `/usr/local/bin/afs-mirror-status.sh`
