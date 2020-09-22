Role to set up Prometheus mysqld_exporter
=========================================

This role will set up a Prometheus mysqld_exporter, running as a Podman
container.

# Variables

The role will use the following variable, defined in the inventory:

* `mysqld_exporter_user`: name of the user that mysqld_exporter will use to
  connect to the database. The user needs at least the PROCESS, REPLICATION CLIENT
  grants on the database.

* `mysqld_exporter_password`: password for the user used to connect to the
  database.

* `mysqld_exporter_use_ssl`: if set to `true`, the exporter will enable SSL
  to connect to the database.

* `mysqld_exporter_ssl_certs` will define the different SSL certificates, if enabled.
  The following structure is needed:

        mysql_exporter_ssl_certs:
          ca-cert:
            filename: "ca-cert.pem"
            contents: |
                -----BEGIN CERTIFICATE-----
                <certificate contents>

  Each entry will have a name, and two keys: `filename` to specify the file
  name, and `contents` with the certificate contents. All certificates included
  here will be stored under `/etc/mysqld_exporter/ssl`.
