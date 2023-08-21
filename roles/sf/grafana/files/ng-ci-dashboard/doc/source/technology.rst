***************
Used technology
***************

For the dashboarding project the following technologies have been used.

=======
Grafana
=======

Grafana is a famous open source analytics and visualization web application. It provides compelling charts, graphs and alerts and supports long list of data sources. Grafana is a popular component in monitoring stack. Grafana is used for the visual part of the dashboards.


==========
Prometheus
==========

Prometheus is an open source application used for event monitoring and alerting. The data (metrics) are stored in time series database. Specific query language is used for accesing the stored time series data. Prometheus is used as the main data source for created dashboards.

=============
Elasticsearch
=============

Elasticsearch is a very popular search engine based on Apache Lucene library. It is capable full-text engine. In dashboarding project it is used for storing an quering zuul jobs data.

=====================
Jsonnet and Grafonnet
=====================

Grafana has quite comfortable web UI for creating dashboards. The created dashboard can be exported to json formatted files which can be stored in SCM. But it is almost impossible to use these files in the review process. Because of this there has been couple of attempts to address this issue.

The most recent and feature reach project is the `grafonnet <https://github.com/grafana/grafonnet/tree/main>`_ [#f1]_ based on `jsonnet <https://jsonnet.org/>`_. The jsonnet is focused on generating json files - addressing the json issues regarding the review process (e.g.code reuse, lack of comments).

The usage of jsonnet and grafonnet for writing dashboards is described in :ref:`dashboard-as-code-label`.


.. rubric:: Footnotes

.. [#f1] The ci-grafana instance in which the hive cluster dashboard is located use `grafonnet-lib <https://github.com/grafana/grafonnet-lib>`_ - an older simmilar project which is now replaced by the grafonnet.
