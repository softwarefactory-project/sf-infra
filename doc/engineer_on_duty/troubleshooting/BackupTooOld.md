BackupTooOld
============

Why is it firing ?
------------------

The alert is ringing because `bup` tool, which is used to create
backup of our infrastructure is outdated.

Solution
--------
If the alert is ringing, you shoud check:

- to get other backup hosts, just type: `export BUP_DIR=/var/lib/backup_lists_rdo/bup ; sudo /usr/local/bin/bup ls`
- last timestamp in `bup` dir, eg.: `export BUP_DIR=/var/lib/backup_lists_rdo/bup ; sudo /usr/local/bin/bup ls -l lists.rdoproject.org`
- logs from ansible job, that is doing backup
- if logs are not available, you can execute ansible for doing new one: `ansible-playbook /var/lib/software-factory/ansible/sf_backup.yml`
- if problems is not in `sf_backup.yml` playbook

```
NOTE:
Some host is using bup in the container, so the same commands to verify if bup
backups are done, you need to execute in the container via: podman exec -it bup /bin/bash.
```

WARNING:
```
It may be a situation, that the bup container is not running and you are not
able to:
```
- create container: `/usr/local/bin/podman-bup.sh`
- even start that container by typing: `podman start bup`
- it raise simillar to this one:
```
error creating container storage: the container name "bup" is already in use
by "ebe753b4c13feb2191f42fb298d465e91b0fd5955895c8be49f851815f3cd6ff". You
have to remove that container to be able to reuse that name.: that name is already in use
```
Problem is related to nfs storage and old podman version. In that case,
you should run:

```
podman rm --storage bup
```

If podman returns you:
```
Error: error removing storage for container "ebe753b4c13feb2191f42fb298d465e91b0fd5955895c8be49f851815f3cd6ff": unlinkat
/var/lib/containers/storage/overlay/ebe753b4c13feb2191f42fb298d465e91b0fd5955895c8be49f851815f3cd6ff/merged: device or resource busy
```

You can try:
```
# try multiple times delete the storage:
$ for i in {1..4}; do
    podman rm bup --storage
    sleep 5
done
```

Finally, if above solution will not work, try:
```
mv /var/lib/containers/storage/overlay/ebe753b4c13feb2191f42fb298d465e91b0fd5955895c8be49f851815f3cd6ff /tmp/
podman rm bup --storage
/usr/local/bin/podman-bup.sh
podman start bup
```
