ElasticsearchOutdatedMetrics
============================

Why is it firing ?
------------------

The last metric that was available in Elasticsearch for index 'logstash-*'
is older that 3 days.


To check
--------

The workflow how Zuul console output is available in Elasticsearch has a
complicated workflow and it require to check multiple services logs:

- logstash logs (not just journalctl -u logstash)
- job-logs-gearman-client
- job-logs-gearman-worker

It can be situation, that logstash service just freeze. Simple service
restart is enough.

When to ignore
--------------

It can be ignored when none CI jobs executed over 3 days.
