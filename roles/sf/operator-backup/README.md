# operator-backup

This role backups a Software Factory Operator's instance, setting it as a cronjob.

It executes the sf-operator's [Backup](https://softwarefactory-project.github.io/sf-operator/deployment/backup-restore/#backup-and-restore) command.

- Requirements:
  go binary [version](https://github.com/softwarefactory-project/sf-operator/blob/master/go.mod#L5)
  kubectl/oc client cli command
  sf-operator's cli command
