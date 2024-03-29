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
  uid: common.dashboardUniqueIds()['podified-promotion-zuul-jobs']
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
          "result",
          "pipeline",
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
        build_type: 12,
        end_time: 5,
        job_name: 0,
        log_url: 6,
        message: 8,
        project: 9,
        pipeline: 10,
        result: 1,
        start_time: 3,
        tenant: 11,
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
        tenant: "Tenant",
        pipeline: "Pipeline"
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
  tablePanel.new('current-podified promotion jobs status')
  + tablePanel.panelOptions.withDescription('Description')
  + tablePanel.queryOptions.withDatasource(datasource)
  + tablePanel.queryOptions.withTargets(zuulCommon.zuulOpensearchTarget(datasource, queryElastic))
  + tablePanel.queryOptions.withInterval('1m')
  + tablePanel.queryOptions.withTimeFrom('48h')
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
      zuulCommon.colWidth('Job', 280),
      zuulCommon.colWidth('Status', 90),
      zuulCommon.colWidth('Project', 100),
      zuulCommon.colWidth('Tenant', 100),
      zuulCommon.colWidth('Start Time', 155),
      zuulCommon.colWidth('End Time', 155),
      zuulCommon.colWidth('Branch', 105),
      zuulCommon.colWidth('Pipeline', 320),
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
dashboard.new('current-podified promotion jobs status')
  + dashboard.withDescription('List of current podified promotion Zuul Jobs')
  + dashboard.withTags('current-podified-zuul-jobs')
  + dashboard.withLinks([linkOverview])
  + dashboard.withPanels([
      zuulJobList,
    ])
  + dashboardUid
