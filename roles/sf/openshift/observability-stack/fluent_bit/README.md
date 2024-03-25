# fluent bit

Fluent bit is a log concentrator and forwarder that can be used with a SF deployment (via sf-operator)
when we don't have privileged access to pods logs on nodes.

Typically this is an alternative when we cannot set up nor configure a Promtail service on nodes where
the SF pods are running. Promtail should **always** be the preferred solution for consistency and completeness.

This role deploys fluent bit and sets up an input endpoint, forwarding to a specified loki instance,
and adds labeling that is as consistent as possible with Promtail's own labeling, meaning the same
promQL queries should be applicable to Promtail and Fluent Bit sources indiscriminately when working
with loki. It relies on [fluent's Fluent Bit Helm chart](https://artifacthub.io/packages/helm/fluent/fluent-bit).