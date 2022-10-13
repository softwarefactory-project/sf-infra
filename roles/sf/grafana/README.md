Grafana README
--------------

Executed commands on infra-summary.json file:

```shell
sed -i 's/kvm13/zm06.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm12/zm05.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm11/zm04.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm10/zm03.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm9/zm02.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm8/zm01.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm7/ze07.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm6/ze06.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm5/ze05.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm4/ze04.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm3/ze03.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm2/ze02.softwarefactory-project.io:9100/Ig' infra-summary.json
sed -i 's/kvm1/ze01.softwarefactory-project.io:9100/Ig' infra-summary.json

sed -i 's/Node Exporter Vexxhost - dpawlik/Node Exporter - Zuul - dpawlik/g' infra-summary.json
sed -i 's/rYdddlP1234/rYdddlP1235/g' infra-summary.json
sed -i 's/prometheus-vexxhost/prometheus/g' infra-summary.json
```
