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

# Container image creation

The official container image built by Prometheus is outdated (as of late September,
2020), so we had to build one under the quay.io/software-factory/mysqld-exporter
namespace. The steps to rebuild the image are:

* First, get the mysqld_exporter source from
  [the GitHub repo](https://github.com/prometheus/mysqld_exporter).

* Then, run `make` locally, to get the mysqld_exporter executable. Unit tests may
  fail if there is no MySQL server active in your machine.

* Check if there is a mysqld_exporter binary under the .build directory. If it is
  only available at the source checkout directory, edit the `Dockerfile` to adapt
  the `COPY` statement.

* Build the container image using `podman build -t mysqld-exporter .`.

* Finally, tag and upload the container image to the registry.
