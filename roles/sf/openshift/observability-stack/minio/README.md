# minio

MinIO provides a temporary solution to enable S3-like object storage for other services and workflows
(observability stack, sf backups, etc).

This role deploys minIO in ["single node, single drive"](https://min.io/docs/minio/linux/operations/install-deploy-manage/deploy-minio-single-node-single-drive.html) mode. The role targets a MicroShift instance
but should work properly on OpenShift as well.
This approach was preferred for its simplicity over using the minIO operator, which had several downsides:

* On MicroShift, deploying the operator required some patching to conform to security context requirements.
* The operator deploys "tenant" resources, which require a minimum of 2 replicas with 2 persistent volumes each
  (for data parity check constraints). This is overkill for our use case and current target deployment environment
  (MicroShift, which is a single node cluster anyway).
* Deploying a tenant resource on MicroShift fails on self-discovery issues, ie the replicas couldn't talk to each other.
  The root issue wasn't determined but the likely culprit is DNS services on MicroShift not being provided in the
  standard kubernetes way (core dns vs openshift dns operator), and the minIO operator expects the DNS resolver to
  be reachable at the core-dns default address.