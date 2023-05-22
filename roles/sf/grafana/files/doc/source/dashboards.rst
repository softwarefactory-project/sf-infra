.. _dashboards-label:

**********************
Dashboards description
**********************




========
Overview
========

Entry point showing overall statistics for prow and zuul jobs and a link to Hive pools dashboard in ci-grafana [#fn1]_.

==================
Hive cluster pools
==================

Dashboard shows the availability of OSP specific Hive pools [#fn1]_. There is a time series panel showing the availability development in time and a statistical panel showing the availability ratio for each pool.

This dashboard is available in `ci-grafana <https://grafana-route-ci-grafana.apps.ci.l2s4.p1.openshiftapps.com/?orgId=1>`_ because the data source is not accessible from outside systems.

Data source
  Prometheus

=========
Prow jobs
=========

Dashboards are showing information about prow jobs testing OSP operators.

Data source
  Prometheus

------------
Success rate
------------

For each operator there is one panel showing the success rate of prow jobs during last 8h. Each of the panel has a link to the corresponding prow page specific to the given operator.

---------------
Prow job detail
---------------

The respective operator and job result can be picked out from the drop down boxes at the top of the dashboard. For the selected operator/result combination the respective time series and success ratio are shown. The time series panel has a link to the prow page showing the OSP operator jobs specific for the selected operator and result.

=========
Zuul jobs
=========

Data source
  Elasticsearch

.. rubric:: Footnotes

.. [#fn1] This dashboard is available in `ci-grafana <https://grafana-route-ci-grafana.apps.ci.l2s4.p1.openshiftapps.com/?orgId=1>`_ because the data source is not accessible from outside systems.
