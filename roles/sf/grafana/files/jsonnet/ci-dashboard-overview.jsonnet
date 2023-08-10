local grafonnet = import 'grafonnet-v9.4.0/main.libsonnet';
local dashboard = grafonnet.dashboard;
local panel = grafonnet.panel;
local statPanel = panel.stat;
local txtPanel = panel.text;
local zuulTotData = 'prometheus';
local prowTotData = 'Prometheus-openshift';
local prometheusQuery = grafonnet.query.prometheus;
local dashLink = grafonnet.dashboard.link;
local queryRate = '8h';

local common = import 'common.jsonnet';

local txtHivepanel =
  |||
    <h1 style="text-align: center;">
      Hive pool utilization
    </h1>
    <p style="text-align: center;">
      The dashboard is available in ci-grafana instance.
    </p>
    <div style="text-align:center">
      Follow this
      <a href="https://grafana-route-ci-grafana.apps.ci.l2s4.p1.openshiftapps.com/d/e690fe42e276cd0fbccaed851464e92bfbe6335e/osp-hive-pools-utilization?orgId=1&refresh=1m)">
        link
      </a>.
    </div>
  |||
;

local linkProwsuccess = common.dashboardLinkTag(
  'Prow jobs operator success rate',
  'prow-success'
);

local linkZuuljobs = common.dashboardLinkTag(
  'Zuul jobs detail',
  'zuul-jobs'
);

local linkGHworkflows = common.dashboardLinkTag(
  'Github workflows',
  'gh-workflows'
);

local hivePanel =
  txtPanel.new('Hive status')
  + txtPanel.gridPos.withH(9)
  + txtPanel.gridPos.withW(12)
  + txtPanel.gridPos.withX(0)
  + txtPanel.gridPos.withY(0)
  + txtPanel.options.withContent(value=txtHivepanel)
;

local zuulPanel =
  statPanel.new('Zuul jobs cummulative succes rate.')
  + statPanel.panelOptions.withDescription('')
  + statPanel.queryOptions.withDatasource(zuulTotData)
  + statPanel.queryOptions.withTargets(common.zuulPrometheusTarget(zuulTotData, queryRate))
  + statPanel.queryOptions.withInterval('1m')
  + statPanel.queryOptions.withTimeFrom('12h')
  + statPanel.standardOptions.withUnit('percent')
  + statPanel.gridPos.withH(9)
  + statPanel.gridPos.withW(12)
  + statPanel.gridPos.withX(12)
  + statPanel.gridPos.withY(0)
  + statPanel.standardOptions.thresholds.withSteps(common.prowThresholds())
;

local prowPanel =
  statPanel.new('Prow jobs cummulative succes rate.')
  + statPanel.panelOptions.withDescription('')
  + statPanel.queryOptions.withDatasource(prowTotData)
  + statPanel.queryOptions.withTargets(common.prowTotalTarget(prowTotData, queryRate))
  + statPanel.queryOptions.withInterval('1m')
  + statPanel.queryOptions.withTimeFrom('12h')
  + statPanel.standardOptions.withUnit('percent')
  + statPanel.gridPos.withH(9)
  + statPanel.gridPos.withW(12)
  + statPanel.gridPos.withX(0)
  + statPanel.gridPos.withY(9)
  + statPanel.standardOptions.thresholds.withSteps(common.prowThresholds())
;

local phPanel1 =
  txtPanel.new('Placeholder 1')
  + txtPanel.gridPos.withH(9)
  + txtPanel.gridPos.withW(6)
  + txtPanel.gridPos.withX(12)
  + txtPanel.gridPos.withY(9)
  + txtPanel.options.withContent(value='<h1 style="text-align: center;">Placeholder 1</h1>')
;

local phPanel2 =
  txtPanel.new('Placeholder 2')
  + txtPanel.gridPos.withH(9)
  + txtPanel.gridPos.withW(6)
  + txtPanel.gridPos.withX(18)
  + txtPanel.gridPos.withY(9)
  + txtPanel.options.withContent(value='<h1 style="text-align: center;">Placeholder 2</h1>')
;

dashboard.new('CI dashboard overview')
  + dashboard.withDescription('POC of nextgen CI dashboard. Trying to cover all the CI process.')
  + dashboard.withTags('overview')
  + dashboard.withLinks([linkProwsuccess, linkZuuljobs, linkGHworkflows])
  + dashboard.withPanels([
      hivePanel,
      zuulPanel,
      prowPanel,
      phPanel1,
      phPanel2,
    ])

