Role to set up Prometheus apache_exporter
=========================================

This role will set up a Prometheus [apache_exporter](https://github.com/Lusitaniae/apache_exporter),
running as a Podman container.

# Variables

The role will use the following variables, defined in the inventory:

* `apache_exporter_url`: this is the URL to the httpd status page, enabled with
  the [server-status module](https://httpd.apache.org/docs/2.4/mod/mod_status.html).
  Remember to use the machine-readable URL, ending with `?auto`. Defaults to
  `http://localhost/server-status?auto`.

* `apache_exporter_insecure`: when set to `true`, do not check the SSL certificate
  when querying the https status page. Defaults to `false`.

# Container image creation

There is no official container image for the repo (as of July, 2021), so we had
to build one under the quay.io/software-factory/apache_exporter
namespace. The steps to rebuild the image are:

* First, get the exporter source from
  [the GitHub repo](https://github.com/Lusitaniae/apache_exporter).

* Then, run `make` locally, to get the executable. Unit tests may fail, but you
  can make them work if you replace instances of `docker` with `podman` in the
  makefile.

* Check if there is an apache_exporter binary under the .build directory. If it is
  only available at the source checkout directory, edit the `Dockerfile` to adapt
  the `COPY` statement.

* Build the container image using `podman build -t apache_exporter .`.

* Finally, tag and upload the container image to the registry.

