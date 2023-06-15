# K1S instance

* create the instance in vars/infra-sf/instances.dhall, then run make to generate all needed files

* when the instance is deployed, connect on *managesf.softwarefactory-project.io* and edit */etc/software-factory/kubeconfig.yaml to add a cluster and a context.

```yaml
- cluster:
    insecure-skip-tls-verify: true
    server: https://$public_ip:9023
  name: $instance_name
...
- context:
    cluster: $instance_name
    user: k1s
  name: /$instance_name/
```

* sync kubeconfig.yaml as root user on managesf instance with:

```bash
ansible-playbook /root/sf-ops/playbooks/sync_k8s_config.yaml
```

* connect to zs to restart nodepool-launcher services (ibm launchers don't have k1s access)
