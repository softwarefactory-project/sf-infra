local grafonnet = import 'grafonnet-v10.4.0/main.libsonnet';
local dashboard = grafonnet.dashboard;
local panel = grafonnet.panel;
local txtPanel = panel.text;
local common = import 'common.jsonnet';

local linkOverview = common.dashboardLinkTag(
  'CI dashboard overview',
  'overview'
);

local dashboardUid = {
  uid: common.dashboardUniqueIds()['osp-ci-upstream-gh-workflows']
};

local workflowUrlTemplate = |||
  <a href='https://github.com/%s/%s/actions/workflows/%s' style='display: block; margin-bottom: 10px; font-size: 16px; text-decoration: none;'>
    <img src='https://github.com/%s/%s/actions/workflows/%s/badge.svg' alt='Cinder Operator image builder" style="max-width: 100%%;'>
  </a>
|||;

local operators = common.operators();

# Not all the operators have the build workflow
local operatorsWithoutBuildWorkflow = [
  'ci-framework',
  'edpm-ansible',
  'install_yamls'
];

local workflowFilesPerOrg = {
  'openstack-k8s-operators': {
    [operator]: ['build-' + operator + '.yaml'] for operator in operators
    if !std.member(operatorsWithoutBuildWorkflow, operator)
  }
};

local workflowsUrls = [
  workflowUrlTemplate % [organization, project, workflowFile, organization, project, workflowFile]
  for organization in std.objectFields(workflowFilesPerOrg)
  for project in std.objectFields(workflowFilesPerOrg[organization])
  for workflowFile in workflowFilesPerOrg[organization][project]
];

local GhWorkflowPanel =
  txtPanel.new('openstack-k8s-operators Workflows Status')
  + txtPanel.gridPos.withH(22)
  + txtPanel.gridPos.withW(10)
  + txtPanel.gridPos.withX(0)
  + txtPanel.gridPos.withY(0)
  + txtPanel.options.withContent(value=std.join("", workflowsUrls))
;

dashboard.new('openstack-k8s-operators Workflows Status')
  + dashboard.withDescription('POC of nextgen CI workflows dashboard')
  + dashboard.withTags('gh-workflows')
  + dashboard.withLinks([linkOverview])
  + dashboard.withPanels([
      GhWorkflowPanel,
  ])
  + dashboardUid
