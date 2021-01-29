# Data missing from Grafana

This can happens when the telegraf logs contains such message:

```bash
$ journalctl -u telegraf
Jan 29 14:54:45 elk.softwarefactory-project.io telegraf[1714]: 2021-01-29T14:54:45Z E! [outputs.influxdb] When writing to [https://elk.softwarefactory-project.io:8086]: received error partial write: field type conflict: input field "value" on measurement "zuul.nodepool.resources.project.github_com/containers/podman.instances" is type integer, already exists as type float dropped=6; discarding points
```

## Drop bad data type metrics

Collect the bad metrics:

```bash
$ journalctl -u telegraf --since yesterday | grep "field type conflict" | awk '{ print $25 }' | sort -u | sed 's/"//g' > ~/type-error.txt
```

Drop the measurements:

```bash
$ for i in $(cat ~/type-error.txt); do echo $i; influx -ssl  -unsafeSsl -username admin -password a8bd837a-b610-4000-9bf6-1605ec6c4b86 -database 'telegraf' -execute 'drop MEASUREMENT "'$i'"'; done
```
