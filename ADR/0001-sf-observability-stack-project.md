---
status: accepted
date: 2024-01-25
---

# SF Observability Stack's nature as a project

## Context and Problem Statement

We are launching an effort to improve observability and monitoring on our infrastructure, following the migration of Software Factory
to an OpenShift-based architecture. We want the stack to also rely on OpenShift orchestration. The question we need to answer with this
ADR is: where should this project live?

## Considered Options

* Be a part of sf-infra
* Be a component of sf-operator
* Create a new repository, and keep configuration aspects in sf-infra

## Decision Outcome

Proposed, chosen option:

* Be a part of sf-infra

This option makes sense in terms of simplicity. It will always be possible to "graduate" the project to an independent repo if we feel like its context changes (especially if we want genericity and reusability).

## Pros and Cons of the Options

### sf observability stack as part of sf-infra

This is similar to what we are doing today to deploy and configure Prometheus and AlertManager. The main difference would be using manifests to deploy in a K8S-based cluster. We would maintain both
deployment definitions and components configuration in sf-infra.

* Good, because this stack is part of our infra, and this is how we've maintained our monitoring infra (prometheus, alertmanager etc) so far.
* Good, because we can focus on our specific infra use cases rather than try and be generic (which implies more complexity).
* Bad, because the stack or parts of it will be deployed several times (upstream, downstream) and thus there is some code duplication to expect between sf-infra projects upstream and downstream. This can
  be countered with good role design from the start.

### sf observability stack as a component of sf-operator

It was considered at a time that Prometheus could be a default component of a SF deployment via the operator. This would expand on this idea and also add AlertManager, Thanos, MinIO, Loki ... to
the operator controller.

* Good, because that would put everything in one place.
* Good, because we could test the observability stack in the CI prior to adding features or making changes.
* Bad, because that would increase the complexity of the operator and associated manifests by **a lot**.
* Bad, because installing operators is not always possible depending on the cluster; and thus we should resort to operators only if it answers a need that can't be covered in any other way.

### sf observability stack as a new public project, configuration in sf-infra

Create a project on our public Gerrit with replication on GitHub/GitLab, with documentation and testing. Sf-infra will clone and deploy, and hold the configuration specifics of the deployment.

* Good, because this way we could avoid code duplication between our infra contexts (upstream and downstream).
* Good, because this has the potential to be of interest for any party deploying Software Factory; whereas sf-infra is really specific to our infrastructure.
* Good, because we could test the stack with SF-Operator in our CI before landing changes.
* Good, because everything specific to sf-infra should only be configuration stuff.
* Bad, because this requires extra efforts in genericity that might not pay off (how many 3rd party users of SF-operator can we expect to have, and out of them how many would use or contribute to the observability stack?)