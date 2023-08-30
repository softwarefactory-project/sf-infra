// dashboard.jsonnet
local grafonnet = import 'grafonnet-v9.4.0/main.libsonnet';
local dashboard = grafonnet.dashboard;
local tablePanel = grafonnet.panel.table;
local transform = grafonnet.panel.table.transformation;
local var = dashboard.variable;
local mapping = tablePanel.valueMapping;
local esQuery = grafonnet.query.elasticsearch;
local datasource = 'opensearch-rdoproject-zuul';
local common = import 'common.jsonnet';
local zuulCommon = import 'zuul-common.jsonnet';

local dashboardUid = {
  uid: common.dashboardUniqueIds()['data-plane-zuul-jobs']
};

local queryElastic = 'pipeline: "openstack-periodic-container" AND build_type: "build"';
local linkOverview = common.dashboardLinkTag(
  'CI dashboard overview',
  'overview'
);

//-- select displayed columns -----------------------------
local selectColumns =
  transform.withId('filterFieldsByName')
  + transform.withOptions({
      include: {
        names: [
          "build_type",
          "end_time",
          "job_name",
          "project",
          "start_time",
          "branch",
          "log_url",
          "tenant",
          "message",
          "result"
        ]
      }
    })
;

local excludeEpochTimeColumns =
  transform.withId('organize')
//-- exclude original time columns ------------------------
  + transform.withOptions({
      excludeByName: {
        end_time: true,
        start_time: true,
      },
    })
;

local reorderColumns =
  transform.withId('organize')
//-- set custom column order ------------------------------
  + transform.withOptions({
      indexByName: {
        "End Time": 4,
        "Start Time": 2,
        branch: 7,
        build_type: 11,
        end_time: 5,
        job_name: 0,
        log_url: 6,
        message: 8,
        project: 9,
        result: 1,
        start_time: 3,
        tenant: 10,
      },
    })
;

local renameColumns =
  transform.withId('organize')
//-- select column aliases --------------------------------
  + transform.withOptions({
      renameByName: {
        branch: "Branch",
        build_type: "Type",
        end_time: "",
        job_name: "Job",
        log_url: "Log URL",
        message: "Message",
        project: "Project",
        result: "Status",
        start_time: "",
        tenant: "Tenant"
      }
    })
;

local setCustom(filterable, displaymode) =
  {
    filterable: filterable,
    displayMode: displaymode
  }
;

//-- Panel with zuul jobs from elastic --------------------
local zuulJobList =
  tablePanel.new('Dataplane Zuul jobs')
  + tablePanel.panelOptions.withDescription('Description')
  + tablePanel.queryOptions.withDatasource(datasource)
  + tablePanel.queryOptions.withTargets(common.zuulOpensearchTarget(datasource, queryElastic))
  + tablePanel.queryOptions.withInterval('1m')
  + tablePanel.queryOptions.withTimeFrom('12h')
  + tablePanel.queryOptions.withTransformations([
      selectColumns,
      zuulCommon.epochToMs("start_time", "Start Time"),
      zuulCommon.epochToMs("end_time", "End Time"),
      reorderColumns,
      renameColumns,
      excludeEpochTimeColumns,
    ])
  + tablePanel.options.withShowHeader(true)
  + tablePanel.options.footer.TableFooterOptions.withEnablePagination(true)
  + zuulCommon.zuulStatusColorMappingOptions()
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
  + tablePanel.fieldConfig.defaults.withCustom(setCustom(true, 'color-text'))
  + tablePanel.gridPos.withH(16)
  + tablePanel.gridPos.withW(23)
  + tablePanel.gridPos.withX(0)
  + tablePanel.gridPos.withY(0)
;

//-- dashboard with zull job panel ------------------------
dashboard.new('Dataplane Zuul jobs list')
  + dashboard.withDescription('List of data plane Zuul Jobs')
  + dashboard.withTags('data-plane-zuul-jobs')
  + dashboard.withLinks([linkOverview])
  + dashboard.withPanels([
      zuulJobList,
    ])
  + dashboardUid
