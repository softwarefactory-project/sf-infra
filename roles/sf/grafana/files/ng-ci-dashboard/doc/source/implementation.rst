********************
Implementation notes
********************




.. _dashboard-as-code-label:

=================
Dashboard as code
=================

------------
Installation
------------

Install the jsonnet according the information available on its `github page <https://github.com/google/go-jsonnet>`_. There is also an rpm package available::

  dnf install golang-github-google-jsonnet

But usually is couple of minor versions behind. After that install the `jsonnet-bundler <https://github.com/jsonnet-bundler/jsonnet-bundler/>`_ for pluggin handling. There is again the corresponding rpm package::

  dnf install golang-github-jsonnet-bundler

Then you need to clone the sf-infra repository and navigate to the files directory::

  git clone "https://softwarefactory-project.io/r/software-factory/sf-infra"
  cd sf-infra/roles/sf/grafana/files

After that the plugins need to be installed with the jsonnet-bundler::

  jb install

Having all this done the dashboards can be compiled from the .jsonnet files to the internal grafana's .json files like::

  jsonnet -J vendor ng-ci-dashboard/jsonnet/ci-dashoard-zuul-jobs.jsonnet

The standard output can be redirected to a file which can be uploaded to Grafana either manually or via API.

-----
Usage
-----

Write the jsonnet code. Here is as example the code for the prow job success dashboard:

.. literalinclude:: /../../../jsonnet/ci-dashboard-prow-success.jsonnet
   :language: jsonnet

Compile the jsonnet code to the json file which Grafana understands::

  jsonnet -J vendor ng-ci-dashboard/jsonnet/ci-dashboard-prow-success.jsonnet > tmp/ci-dashboard-prow-success.json


Upload the generated json file to Grafana. You can use the manual approach via the Grafana UI or the REST API. Basic hints are shown bellow. You are going to need the authentication token and the folderId (the folderId 0 always exists but the given token might be missing the import permission)::

  DASHBOARD=$(cat tmp/ci-dashboard-prow-success.json)
  curl -X POST \
     -H "Accept: application/json" \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer <Grafana Token>" \
     -d "{\"dashboard\": ${DASHBOARD}, \"folderId\": <folderId>, \"overwrite\": true}" \
     https://<grafana-url>/api/dashboards/import

