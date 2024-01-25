---
status: accepted
date: 2024-01-25
---

# Format of the SF Observability Stack

## Context and Problem Statement

We are launching an effort to improve observability and monitoring on our infrastructure, following the migration of Software Factory
to an OpenShift-based architecture. We want the stack to also rely on OpenShift orchestration. The question we need to answer with this
ADR is: under what form should this project be developed?

## Considered Options

### Where should the project live?

* Manifests (hard coded or templated with kustomize) deployed through Ansible or another form of automation
* Helm project

## Decision Outcome

Proposed outcome:

* Manifests that we can adapt with kustomize

## Pros and Cons of the Options

### Deploying manifests through Ansible

* Good, because the architecture is relatively straightforward so we wouldn't need to maintain a crazy amount of manifests.
* Good, because that's kind of what we're already doing. We have experience with Ansible and its templating system.
* Bad, because we might need to write everything (manifest templates, playbooks) from scratch.

### sf observability stack as a Helm project

* Good, because Helm is the equivalent of a package manager for Kubernetes. This means we can reuse existing charts to deploy the components we need. This should also simplify deployments.
* Good, because this is an opportunity to learn an industry standard - Helm is ubiquitous in the K8s landscape.
* Bad, because many Helm charts need tweaking to work on OpenShift, and even more so on MicroShift. There is a possibility we might not be able to reuse some charts, canceling pro argument 1. Experimentation, however, showed that operators for the components constituting the observability stack can be installed successfully on MicroShift (see section below)

## More Information

### Installing helpful operators on MicroShift

We could require the presence of some operators on the target cluster, which would make the Helm chart even simpler. The following operators have been installed successfully on a microshift.dev instance "by hand", and could thus be used:

#### grafana

1. install via operatorhub https://operatorhub.io/operator/grafana-operator

`Grafana`, `dashboard` and `datasources` resources are then available.

#### prometheus

The operator is required for sf-operator.

### Installing loki without "using" the official Helm chart

We have a simple manifest to deploy a monolithic Loki in the sf-operator repository's `tools/loki` folder. If we want a scalable instance, we can generate a more complete series of manifests with `helm template --values values.yaml loki --namespace=sf-observe grafana/loki > loki-install.yaml` and hack out only the bits we need, then reintegrate them in our Helm chart.