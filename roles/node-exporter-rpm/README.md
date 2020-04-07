Role to deploy Prometheus Node Exporter via RPM
===============================================

This role will take care of configuring Node Exporter on a server, using an
RPM deployed from a Copr repository.

# Variables:

* `prometheus_monitored_services`: this needs to be a list. When specified,
  it will enable systemd service monitoring on the Node Exporter, and
  whitelist the specified services, so they can be monitored.
  NOTE: it is not required to add '.service' suffix on the end of the service name.
