// ci-dashboard-prow-success.jsonnet
local grafonnet = import 'grafonnet-v9.4.0/main.libsonnet';
local common = import 'common.jsonnet';

local dashboard = grafonnet.dashboard;
local statPanel = grafonnet.panel.stat;
local datasource = 'Prometheus-openshift';
local dashLink = grafonnet.dashboard.link;
local operators = common.operators();
local queryRate = '8h';
local prowPanelH = 4;
local prowPanelW = 4;

local dashboardUid = {
  uid: common.dashboardUniqueIds()['prow-success']
};

//-- Link to the overview dashboard -----------------------
local linkOverview = common.dashboardLinkTag(
  'CI dashboard overview',
  'overview'
);

//-- Link to the prow detail dashboard --------------------
local linkProwdetail = common.dashboardLinkTag(
  'Prow job detail',
  'prow-detail'
);

//-- Link to the respective prow page ---------------------
local prowLink(variableStem) = {
  url: std.format(
    'https://prow.ci.openshift.org/?job=pull-ci-openstack-k8s-operators-%s*',
    variableStem
  ),
  title: 'Prow link'
};

//-- Panel for one operator -------------------------------
local jobSuccess(title, description, variableStem, x=0, y=0) =
  statPanel.new(title + ' ' + variableStem)
  + statPanel.panelOptions.withDescription(description + ' ' + variableStem)
  + statPanel.queryOptions.withDatasource(datasource)
  + statPanel.queryOptions.withTargets(common.prowTarget(datasource, variableStem, 'success'))
  + statPanel.queryOptions.withInterval('1m')
  + statPanel.queryOptions.withTimeFrom('12h')
  + statPanel.standardOptions.withUnit('percent')
  + statPanel.gridPos.withH(prowPanelH)
  + statPanel.gridPos.withW(prowPanelW)
  + statPanel.gridPos.withX(x)
  + statPanel.gridPos.withY(y)
  + statPanel.standardOptions.thresholds.withSteps(common.prowThresholds())
  + statPanel.standardOptions.withLinks(prowLink(variableStem))
;

//-- Dashboard with panels for all operators --------------
dashboard.new('Prow jobs operator success rate')
  + dashboard.withDescription('Prow job success rate for OSP operators.')
  + dashboard.withTags('prow-success')
  + dashboard.withLinks([linkOverview, linkProwdetail])
  + dashboard.withPanels([
    jobSuccess(
      'Prow operators',
      'Prow job operators success rate',
      operators[i],
      (i%6)*prowPanelW, (i/6|0)*prowPanelH
    )
    for i in std.range(0, std.length(operators)-1)
  ])
  + dashboardUid
