local grafonnet = import 'grafonnet-v9.4.0/main.libsonnet';
local prometheusQuery = grafonnet.query.prometheus;
local dashLink = grafonnet.dashboard.link;

{
  dashboardUniqueIds():
    {
      'gh-workflows': std.md5('gh-workflows'),
      'overview': std.md5('overview'),
      'prow-detail': std.md5('prow-detail'),
      'prow-success': std.md5('prow-success'),
      'zuul-jobs': std.md5('zuul-jobs')
    },

  operators():
    ['ci-framework',
     'cinder-operator',
     'dataplane-operator',
     'edpm-ansible',
     'glance-operator',
     'horizon-operator',
     'infra-operator',
     'install_yamls',
     'ironic-operator',
     'keystone-operator',
     'manila-operator',
     'mariadb-operator',
     'neutron-operator',
     'nova-operator',
     'octavia-operator',
     'openstack-ansibleee-operator',
     'openstack-baremetal-operator',
     'openstack-operator',
     'osp-director-operator',
     'ovn-operator',
     'placement-operator',
     'telemetry-operator'
    ],

  queryString(variableStem, state):
    std.format('prowjobs{job_name=~"pull-ci-openstack-k8s-operators-%s-.*", state=~"%s"}', [variableStem, state]),

  queryStringRate(variableStem, state, rate):
    'sum(rate(' + self.queryString(variableStem, state) + '[' + rate + ']))',

  queryStringRatio(variableStem, state, rate):
    self.queryStringRate(variableStem, state, rate) + '/' +
    self.queryStringRate(variableStem, 'success|aborted|failure', rate) + '*100',

  prowTarget(datasource, variableStem, result, queryRate='8h'):
    prometheusQuery.new(
      datasource,
      self.queryStringRatio(variableStem, result, queryRate)
    ),

  prowTotalQuery(rate):
    std.format(
      |||
        sum(
          rate(
            prowjobs{
              job_name=~"pull-ci-openstack-k8s-operators-.*", state="success"
            }[%s]
          )
        )/
        sum(
          rate(
            prowjobs{
              job_name=~"pull-ci-openstack-k8s-operators-.*", state=~"success|aborted|failure"
            }[%s]
          )
        )*100
      |||,
      [rate, rate]
    ),

  prowTotalTarget(datasource, queryRate):
    prometheusQuery.new(
      datasource,
      self.prowTotalQuery(queryRate)
    ),

  zuulQuery(rate):
    std.format(
      |||
        sum (
          rate(
            zuul_nodepool_requests_state_by_label{
              job="statsd_exporter",state="fulfilled"
            }[%s]
          )
        )/
        sum(
          rate(
            zuul_nodepool_requests_state_by_label{
              job="statsd_exporter",state=~"canceled|fulfilled|requested|failed"
            }[%s]
          )
        )*100
      |||,
      [rate, rate]
    ),

  zuulPrometheusTarget(datasource, queryRate):
    prometheusQuery.new(
      datasource,
      self.zuulQuery(queryRate)
    ),

  prowThresholds():
    [
      {value: 0, color: 'red'},
      {value: 75, color: 'yellow'},
      {value: 85, color: 'green'},
   ],

  dashboardLinkUrl(title, url):
    dashLink.link.new(title, url)
    + dashLink.link.withIcon('dashboard'),

  dashboardLinkTag(title, tag):
    dashLink.dashboards.new(title, tag),

}
