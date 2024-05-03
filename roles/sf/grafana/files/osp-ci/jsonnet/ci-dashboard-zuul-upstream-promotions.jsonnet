// dashboard.jsonnet
local grafonnet = import 'grafonnet-v10.4.0/main.libsonnet';
local dashboard = grafonnet.dashboard;
local tablePanel = grafonnet.panel.table;
local pieChartPanel = grafonnet.panel.pieChart;
local gaugePanel = grafonnet.panel.gauge;
local transform = grafonnet.panel.table.transformation;
local var = dashboard.variable;
local mapping = tablePanel.valueMapping;
local esQuery = grafonnet.query.elasticsearch;
local datasource = 'opensearch-rdoproject-zuul';
local common = import 'common.jsonnet';
local zuulCommon = import 'zuul-common.jsonnet';

local dashboardUid = {
  uid: common.dashboardUniqueIds()['osp-ci-zuul-openstack-upstream-promotions'],
};

local pipelines = [
  'openstack-periodic-container-master-centos9',
  'openstack-periodic-container-antelope-centos9',
  'openstack-operators-periodic-integration-antelope-centos9',
];

local rowsToFields =
  transform.withId('rowsToFields')
  + transform.withOptions({});

local tableInitialYPos = 0;
local gaugeInitialYPos = 10;

local panelConfigs = [
  [
    // Table panels https://grafana.github.io/grafonnet/API/panel/table/index.html
    tablePanel.new('Pipeline: ' + pipeline + ' jobs zuul jobs')
    + tablePanel.panelOptions.withDescription('Jobs under the Pipeline: ' + pipeline + ' jobs')
    + tablePanel.queryOptions.withDatasource(
      'lucene',
      datasource
    )
    + tablePanel.queryOptions.withTargets(zuulCommon.zuulOpensearchTarget(datasource, 'pipeline.keyword: "' + pipeline + '" AND build_type: "build"'))
    + tablePanel.queryOptions.withInterval('1m')
    + tablePanel.queryOptions.withTransformations([
      common.selectTableColumns(),
      zuulCommon.epochToMs('start_time', 'Start Time'),
      zuulCommon.epochToMs('end_time', 'End Time'),
      common.reorderTableColumns(),
      common.renameColumns(),
      common.excludeEpochTimeColumns(),
    ])
    + tablePanel.options.withShowHeader(true)
    + tablePanel.options.footer.TableFooterOptions.withEnablePagination(true)
    + zuulCommon.zuulStatusColorMappingOptions(tablePanel)
    + zuulCommon.zuulDatesColorNeutral(tablePanel)
    + tablePanel.standardOptions.withOverrides([
      zuulCommon.colWidth('Job', 250),
      zuulCommon.colWidth('Status', 90),
      zuulCommon.colWidth('Start Time', 155),
      zuulCommon.colWidth('End Time', 155),
      zuulCommon.colWidth('Branch', 105),
      zuulCommon.colUnit('Start Time', 'dateTimeAsIso'),
      zuulCommon.colUnit('End Time', 'dateTimeAsIso'),
      zuulCommon.colUrl('Log URL'),
    ])
    + tablePanel.fieldConfig.defaults.custom.withFilterable(value=true)
    + tablePanel.fieldConfig.defaults.custom.withDisplayMode('color-text')
    + tablePanel.panelOptions.withGridPos(
      h=10,
      w=24,
      x=0,
      y=if index == 0 then tableInitialYPos else (tableInitialYPos + (index * 15))
    ),
    // Pie chart panels https://grafana.github.io/grafonnet/API/panel/pieChart/index.html
    pieChartPanel.new('Pipeline: ' + pipeline + ' jobs Chart')
    + pieChartPanel.panelOptions.withDescription('Jobs under the Pipeline: openstack-promote-component jobs chart percents')
    + pieChartPanel.queryOptions.withDatasource(
      'lucene',
      datasource
    )
    + pieChartPanel.queryOptions.withTargets(zuulCommon.zuulOpensearchTarget(datasource, 'pipeline.keyword: "' + pipeline + '" AND build_type: "build"'))
    + pieChartPanel.queryOptions.withInterval('1m')
    + pieChartPanel.queryOptions.withTransformations([
      common.selectPieChartColumns(),
      zuulCommon.epochToMs('start_time', 'Start Time'),
      zuulCommon.epochToMs('end_time', 'End Time'),
      common.reorderPieChartColumns(),
      common.renamePieChartColumns(),
      common.groupPieChartByColumns(),
      rowsToFields,
    ])
    + pieChartPanel.standardOptions.withOverrides(zuulCommon.pieChartStatusColorMapping())
    + pieChartPanel.standardOptions.withFilterable(value=true)
    + pieChartPanel.panelOptions.withGridPos(
      h=5,
      w=12,
      x=0,
      y=if index == 0 then gaugeInitialYPos else (gaugeInitialYPos + (index * 15))
    )
    + pieChartPanel.options.withReduceOptions({
      values: false,
      calcs: ['lastNotNull'],
      fields: '',
    })
    + pieChartPanel.options.withPieType('pie')
    + pieChartPanel.options.withTooltip(
      {
        mode: 'single',
      }
    )
    + pieChartPanel.options.legend.withDisplayMode('list')
    + pieChartPanel.options.legend.withPlacement('right')
    + pieChartPanel.options.legend.withValues([]),
    // Gauge Panels https://grafana.github.io/grafonnet/API/panel/gauge/index.html
    gaugePanel.new('Pipeline: ' + pipeline + ' jobs Gauge')
    + gaugePanel.panelOptions.withDescription('Jobs under the Pipeline: ' + pipeline + ' jobs gauge')
    + gaugePanel.queryOptions.withDatasource(
      'lucene',
      datasource
    )
    + gaugePanel.queryOptions.withTargets(zuulCommon.zuulOpensearchTarget(datasource, 'pipeline.keyword: "' + pipeline + '" AND build_type: "build"'))
    + gaugePanel.queryOptions.withInterval('1m')
    + gaugePanel.queryOptions.withTransformations([
      common.selectPieChartColumns(),
      zuulCommon.epochToMs('start_time', 'Start Time'),
      zuulCommon.epochToMs('end_time', 'End Time'),
      common.reorderPieChartColumns(),
      common.renamePieChartColumns(),
      common.groupPieChartByColumns(),
      rowsToFields,
    ])
    + gaugePanel.standardOptions.withOverrides(zuulCommon.pieChartStatusColorMapping())
    + gaugePanel.standardOptions.withFilterable(value=true)
    + gaugePanel.panelOptions.withGridPos(
      h=5,
      w=12,
      x=12,
      y=if index == 0 then gaugeInitialYPos else (gaugeInitialYPos + (index * 15))
    )
    + gaugePanel.options.withReduceOptions({
      values: false,
      calcs: ['lastNotNull'],
      fields: '',
    })
    + gaugePanel.options.withOrientation('auto'),

  ]
  for index in std.range(0, std.length(pipelines) - 1)
  for pipeline in [pipelines[index]]
];

// Main dashboard that will have all the visualizations
dashboard.new('Openstack Upstream Releases Promotions')
+ dashboard.withDescription('List of Zuul Jobs under different upstream Openstack releases promotion pipelines')
+ dashboard.withTags('zuul-jobs-upstream-promotions')
+ dashboard.withPanels(
  [panel for panels in panelConfigs for panel in panels],
  setPanelIDs=true
)
+ dashboard.time.withFrom('now-24h')
+ dashboard.time.withTo('now')
+ dashboardUid
