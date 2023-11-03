Jsonnet-Grafonnet Docker Image
==============================

This Docker image provides a convenient environment for using Jsonnet
and Grafonnet to generate JSON output for Grafana dashboards.

Building the Docker Image
-------------------------

To build the Docker image:

   .. code-block:: bash

      docker build -t jsonnet-processor .

Usage
-----

Generating Grafana Dashboard JSON.

To generate JSON from a jsonnet file we will need to mount the jsonnet specific file or folder and pass as parameter the jsonnet file we want to convert:

.. code-block:: bash

   docker run -v $(pwd)/jsonnet/:/app/jsonnet jsonnet-processor jsonnet/ci-dashboard-gh-workflows.jsonnet

This will output the JSON model.

Example:

.. code-block:: bash
    docker run -v $(pwd)/jsonnet/:/app/jsonnet jsonnet-processor jsonnet/ci-dashboard-gh-workflows.jsonnet
    {
       "description": "POC of nextgen CI workflows dashboard",
       "links": [
          {
             "tags": [
    ...
