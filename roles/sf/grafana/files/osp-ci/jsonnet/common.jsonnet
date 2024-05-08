local grafonnet = import 'grafonnet-v10.4.0/main.libsonnet';
local prometheusQuery = grafonnet.query.prometheus;
local dashLink = grafonnet.dashboard.link;
local transform = grafonnet.panel.table.transformation;

{
  dashboardUniqueIds():
    {
      'osp-ci-upstream-dashboard-overview': std.md5('osp-ci-upstream-dashboard-overview'),
      'osp-ci-zuul-openstack-upstream-promotions': std.md5('osp-ci-zuul-openstack-upstream-promotions'),
      'osp-ci-upstream-gh-workflows': std.md5('osp-ci-upstream-gh-workflows'),
    },

  operators():
    [
      'ci-framework',
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
      'telemetry-operator',
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

  prowPageLink(operator, result):
    {
      url: std.format(
        'https://prow.ci.openshift.org/?job=pull-ci-openstack-k8s-operators-%s*&state=%s',
        [operator, result]
      ),
      title: std.format(
        'Prow %s operator %s status link',
        [operator, result]
      ),
    },

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
      { value: 0, color: 'red' },
      { value: 75, color: 'yellow' },
      { value: 85, color: 'green' },
    ],

  dashboardLinkUrl(title, url):
    dashLink.link.new(title, url)
    + dashLink.link.withIcon('dashboard'),

  dashboardLinkTag(title, tag):
    dashLink.dashboards.new(title, tag),

  // Columns Renaming to avoid having same fields than Opensearch
  renameColumns():
    transform.withId('organize')
    + transform.withOptions({
      renameByName: {
        branch: 'Branch',
        end_time: '',
        job_name: 'Job',
        log_url: 'Log URL',
        message: 'Message',
        project: 'Project',
        result: 'Status',
        start_time: '',
        tenant: 'Tenant',
      },
    }),


  excludeEpochTimeColumns():
    transform.withId('organize')
    + transform.withOptions({
      excludeByName: {
        end_time: true,
        start_time: true,
      },
    }),

  selectTableColumns():
    transform.withId('filterFieldsByName')
    + transform.withOptions({
      include: {
        names: [
          'end_time',
          'job_name',
          'project',
          'start_time',
          'branch',
          'log_url',
          'tenant',
          'message',
          'result',
        ],
      },
    }),

  selectPieChartColumns():
    transform.withId('filterFieldsByName')
    + transform.withOptions({
      include: {
        names: [
          'job_name',
          'result',
        ],
      },
    }),

  reorderPieChartColumns():
    transform.withId('organize')
    + transform.withOptions({
      indexByName: {
        job_name: 0,
        result: 1,
      },
    }),

  renamePieChartColumns():
    transform.withId('organize')
    + transform.withOptions({
      renameByName: {
        job_name: 'Job',
        result: 'Status',
      },
    }),

  groupPieChartByColumns():
    transform.withId('groupBy')
    + transform.withOptions({
      fields: {
        Job: {
          operation: 'aggregate',
          aggregations: ['count'],
        },
        Status: {
          operation: 'groupby',
          aggregations: [],
        },
      },
    }),

  reorderTableColumns():
    transform.withId('organize')
    + transform.withOptions({
      indexByName: {
        'End Time': 4,
        'Start Time': 2,
        branch: 7,
        end_time: 5,
        job_name: 0,
        log_url: 6,
        message: 8,
        project: 9,
        result: 1,
        start_time: 3,
        tenant: 10,
      },
    }),
}
