// dashboard.jsonnet
local grafonnet = import 'grafonnet-v9.4.0/main.libsonnet';
local dashboard = grafonnet.dashboard;
local statPanel = grafonnet.panel.stat;
local tsPanel = grafonnet.panel.timeSeries;
local var = dashboard.variable;
local datasource = 'Prometheus-openshift';
local prometheusQuery = grafonnet.query.prometheus;
local dashLink = grafonnet.dashboard.link;
local queryRate = '8h';

local common = import 'common.jsonnet';
local operators = common.operators();

local linkProwsuccess = common.dashboardLinkTag(
  'Prow jobs operator success rate',
  'prow-success'
);

local operatorVar =
  var.custom.new(
    'operator',
    values=operators,
  );

local resultVar =
  var.custom.new(
    'result',
    values=[
      'success',
      'aborted',
      'failure',
      'pending',
    ]
  );

local prowJobLink = {
  title: 'Prow page',
  url: 'https://prow.ci.openshift.org/?job=pull-ci-openstack-k8s-operators-$operator*&state=$result',
  targetBlank: true
};

local tsQuery =
  |||
    sum by (job_name) (
      rate(
        prowjobs{
          job_name=~"pull-ci-openstack-k8s-operators-$operator-.*",
          state="$result"
        }[8h]
      )
    )*8*60*60
  |||
;

local tsTarget =
  prometheusQuery.new(
    datasource,
    tsQuery
  )
  + prometheusQuery.withLegendFormat("{{job_name}}");


local jobDetail =
  tsPanel.new('Prow $operator jobs $result detail')
  + tsPanel.panelOptions.withDescription('Number of jobs for the selected operator with the selected result')
  + tsPanel.queryOptions.withDatasource(datasource)
  + tsPanel.queryOptions.withTargets(tsTarget)
  + tsPanel.queryOptions.withInterval('1m')
  + tsPanel.queryOptions.withTimeFrom('12h')
  + tsPanel.gridPos.withH(14)
  + tsPanel.gridPos.withW(17)
  + tsPanel.gridPos.withX(0)
  + tsPanel.gridPos.withY(0)
  + tsPanel.options.legend.withDisplayMode('table')
  + tsPanel.options.legend.withPlacement('right')
  + tsPanel.panelOptions.withLinks([prowJobLink])
;

local jobStat =
  statPanel.new('Prow $operator $result rate')
  + statPanel.queryOptions.withDatasource(datasource)
  + statPanel.queryOptions.withTargets(common.prowTarget(datasource, '$operator', '$result'))
  + statPanel.queryOptions.withTimeFrom('12h')
  + statPanel.standardOptions.withUnit('percent')
  + statPanel.gridPos.withH(14)
  + statPanel.gridPos.withW(7)
  + statPanel.gridPos.withX(17)
  + statPanel.gridPos.withY(0)
  + statPanel.standardOptions.thresholds.withSteps(common.prowThresholds())
;

dashboard.new('Prow jobs operator detail')
  + dashboard.withDescription('Detailed look on prow jobs building and testing openstack operators')
  + dashboard.withTags('prow-detail')
  + dashboard.withLinks([linkProwsuccess])
  + dashboard.withVariables([
      operatorVar,
      resultVar
    ])
  + dashboard.withPanels([
      jobDetail,
      jobStat,
    ])
