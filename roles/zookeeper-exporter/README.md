Role to deploy Prometheus Zookeeper exporter
============================================

This role will take care of configuring Zookeeper Exporter on a server, using an
RPM deployed from a Copr repository.

# Variables:

* `listen-port`:  The address on which to expose the web interface and
                  generated Prometheus metrics. Defaults to: 9141.
* `zookeeper_hosts`: Comma separated list of zookeeper servers.
                     Defaults to: localhost: 2181
* `zookeeper_cert`: The TLS certiticate for Zookeeper client authentication.
                    Defaults to: '/etc/zookeeper/certs/localCA.pem'
* `zookeeper_key`: The TLS key for Zookeeper client authentication.
                   Defaults to: '/etc/zookeeper/certs/localCAkey.pem'
* `zookeeper_tls_enabled`: Enable TLS authentication. Defaults to: true
