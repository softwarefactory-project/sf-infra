Role to deploy Prometheus StatsD Exporter
=========================================

This role will take care of configuring StatsD Exporter on a server, using an
RPM deployed from a Copr repository.

# Variables:

* `listen-address`:  The address on which to expose the web interface and
                     generated Prometheus metrics. Defaults to 9102.
* `listen-udp-port`: The UDP address on which to receive statsd metric lines.
                     "" disables it. Defaults to 9125.
* `listen-tcp-port`: The TCP address on which to receive statsd metric lines.
                     "" disables it. Defaults to 9125.
* `map-config-file-path`: Path to the metric mapping configuration.
                          Defaults to /etc/prometheus/statsd_mapping.yml.
* `statsd_host`: Host on which statsd_exporter is running
