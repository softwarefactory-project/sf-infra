# Observability Stack

The roles and helpers in this folder are meant to facilitate the deployment of an improved observability stack
on our infra(s).

A simplified version of the stack's arch can be seen in `observability-stack-simplified-arch.svg`.

## Description

The stack consists of:

* An object storage
* Loki for log aggregation
* Prometheus + Thanos for metrics collection and long term storage. This Prometheus instance exists alongside the already deployed "Promethei",
  and collects their metrics through the federation endpoint to centralize them.
* Promtail services running on every managed server to collect and forward to Loki journal logs, and if relevant, MicroShift pod logs
* Fluent Bit for log collecting and forwarding, for SF deployments (through sf-operator) on clusters where we cannot deploy Promtail
* Grafana for dashboards and alerts management, with Prometheus and Loki configured as data sources

## Expected improvements over the current monitoring tools

* Aggregated logs in one place, which will also make it easier to correlate events
* Long term storage
* One single place to observe the infrastructure's metrics and logs (grafana dashboards)
* Managing alerts through Grafana will allow us to create alerts not just on metrics but also on log events (for example hitting github API rate limiting)