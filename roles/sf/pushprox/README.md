Role to set up a PushProx proxy or client
=========================================

This role will setup a [PushProx-based][1] proxy or client for Prometheus.
This is required when we want to monitor a system that is located behind
a NAT or firewall, so the Prometheus server cannot connect directly to it
in order to get its metrics.

# Variables

The role will use the following variables, defined in the inventory:

* `pushprox_type`, which can be set to "proxy" or "client".
* `pushprox_proxy_url`, which is only used when `pushproxy_type` is set to
  "client", and defines the URL to connect to.
* `pushprox_proxy_listen`, which is only used when `pushproxy_type` is set to
  "proxy", and defines the listen address for the proxy.

[1]: https://github.com/RobustPerception/PushProx
