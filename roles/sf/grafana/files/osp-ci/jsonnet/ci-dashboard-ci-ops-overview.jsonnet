// dashboard.jsonnet
local grafonnet = import 'grafonnet-v10.4.0/main.libsonnet';
local dashboard = grafonnet.dashboard;
local textPanel = grafonnet.panel.text;

local common = import 'common.jsonnet';

local dashboardUid = {
  uid: common.dashboardUniqueIds()['osp-ci-overview'],
};


local panelTextDescription = |||
  <img src="https://dfg-cre-sandbox.usersys.redhat.com/eodweb/static/img/OSP-DevOps.png" width="75" height="75" />

  <hr class="dashed">

  Hey there 👋. We have some dashboards that can be useful for monitoring several pipelines under the Upstream Zuul CI.

  We are covering:

  <b><a href="https://prometheus.monitoring.softwarefactory-project.io/grafana/d/7ff52dc9be88f4f31afce921e6556212/">Periodic Integration & Container Per Release</a></b>

  <ul>
      <li>openstack-periodic-container-master-centos9</li>
      <li>openstack-periodic-container-antelope-centos9</li>
      <li>openstack-operators-periodic-integration-antelope-centos9</li>
  </ul>

  <b><a href="https://prometheus.monitoring.softwarefactory-project.io/grafana/d/3833bc9e39633bbf2454305849213f0b/">openstack-k8s-operators GitHub Image Builder Workflows</a></b>

  <ul>
      <li>cinder-operator</li>
      <li>dataplane-operator</li>
      <li>glance-operator</li>
      <li>horizon-operator</li>
      <li>infra-operator</li>
      <li>ironic-operator</li>
      <li>keystone-operator</li>
      <li>manila-operator</li>
      <li>mariadb-operator</li>
      <li>neutron-operator</li>
      <li>nova-operator</li>
      <li>octavia-operator</li>
      <li>openstack-ansibleee-operator</li>
      <li>openstack-baremetal-operator</li>
      <li>openstack-operator</li>
      <li>osp-director-operator</li>
      <li>ovn-operator</li>
      <li>placement-operator</li>
      <li>telemetry-operator</li>
  </ul>

  <hr class="dashed">

  <p>👀 Dashboards developed by the CI Operations team.</p>

  <p>Note: The data rendered in the dashboards depends of the Opensearch instance and the pattern name of the jobs. If a new job doesn't follow the same pattern name then it won't be available.</p>
|||;


local panel = [
  textPanel.new('OSP CI Dashboards')
  + textPanel.panelOptions.withDescription('OSP CI Upstream Dashboards')
  + textPanel.panelOptions.withGridPos(
    h=24,
    w=24,
    x=0,
    y=0
  )
  + textPanel.options.withContent(value=panelTextDescription),
];

dashboard.new('CI Ops Overview Dashboard')
+ dashboard.withDescription('Dashboards developed by the OSP CI team.')
+ dashboard.withTags('ci-ops-overview-dashboard')
+ dashboard.withPanels(
  panel,
  setPanelIDs=true
)
+ dashboardUid
