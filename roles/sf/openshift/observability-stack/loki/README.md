# loki

This role deploys Loki's main components (backend, read, write) and a proxy gateway on OpenShift, with MicroShift
intended as a specific target. This architecture is meant to be scalable if needed, over the much simpler "monolith"
deployment currently used in sf-operator's CI.

The manifests were extracted from the loki Helm chart, as it installs by default a lot of components and resources
we don't have a use for.

## Working with the Helm chart

If you need to generate manifests from the Helm chart, follow these steps:

1. install the grafana repo: `helm install grafana https://grafana.github.io/helm-charts
2. create a dummy `values.yaml` file to enable S3 storage and the scalable arch:

```yaml
loki:
  storage:
    bucketNames:
      chunks: chunks
      ruler: ruler
      admin: admin
    type: s3
    s3:
      endpoint: <endpoint>
      region: <AWS region>
      secretAccessKey: <AWS secret access key>
      accessKeyId: <AWS access key ID>
      s3ForcePathStyle: false
      insecure: false
```

3. Generate the manifests

```sh
helm template --values values.yaml loki --namespace=sf-observability grafana/loki > loki-install.yaml
```
4. Weed out the unneeded resources