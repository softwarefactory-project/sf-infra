# Prometheus / Thanos

Prometheus is a metrics collector. Thanos extends Prometheus to provide long-term storage of metrics
on a S3-like object storage. Thanos Query replaces Prometheus as the endpoint to query and graph metrics,
especially the long-term stored ones.

This role deploys a Prometheus resource via the prometheus operator, which is thus a prerequisite on the target
cluster (which should be the case for all the clusters we manage). It is meant to connect to our existing
"Promethei" via their federation endpoints to gather their collected metrics, but ultimately we may want to
reduce the footprint and simply add our current scrape targets to this specific resource (the operator provides
CRs to do so).