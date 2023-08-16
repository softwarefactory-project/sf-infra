// dashboard.jsonnet
local grafonnet = import 'grafonnet-v9.4.0/main.libsonnet';
local dashboard = grafonnet.dashboard;
local tablePanel = grafonnet.panel.table;
local transform = grafonnet.panel.table.transformation;
local var = dashboard.variable;
local override = tablePanel.fieldOverride.byName;
local mapping = tablePanel.valueMapping;
local esQuery = grafonnet.query.elasticsearch;
local datasource = 'opensearch-rdoproject-zuul';
local common = import 'common.jsonnet';

local dashboardUid = {
  uid: common.dashboardUniqueIds()['zuul-jobs']
};

local queryElastic = 'project: "openstack-k8s-operators/*" AND build_type: "build"';
local linkOverview = common.dashboardLinkTag(
  'CI dashboard overview',
  'overview'
);

//-- opensearch target ------------------------------------
local zuulOpensearchTarget(datasource, query) =
  esQuery.withQuery(query)
  + esQuery.withDatasource(datasource)
  + esQuery.withMetrics({type:"raw_data",settings:{size:"500",order:"desc",useTimeRange:true}})
  + esQuery.withTimeField('@timestamp')
  + esQuery.withQueryType('lucene')
  + esQuery.withBucketAggs([])
;

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

local epochToMs(fieldName, alias) =
  transform.withId('calculateField')
  + transform.withOptions({
      alias: alias,
      binary: {
        left: fieldName,
        operator: "*",
        reducer: "sum",
        right: "1000"
      },
      mode: "binary",
      reduce: {
        reducer: "sum"
      }
    })
;

//-- function for custom column width ---------------------
local colWidth(name, width) =
  override.new(name)
  + override.withProperty('custom.width', width)
;

//-- function for custom column unit ----------------------
local colUnit(name, unit) =
  override.new(name)
  + override.withProperty('unit', unit)
;

//-- turn the cell content to http link -------------------
local colUrl(name, targetblank=true) =
  override.new(name)
  + override.withProperty('links', [{targetBlank: targetblank, title: "", url: "${__value.text}"}])
;

//-- change color for specific text -----------------------
local mapNameColors(name, color) =
  mapping.RegexMap.options.withPattern(name)
  + mapping.RegexMap.options.result.withColor(color)
  + mapping.RegexMap.withType('regex')
;

//-- set filterable columns and allow colored text --------
local setCustom(filterable, displaymode) =
  {
    filterable: filterable,
    displayMode: displaymode
  }
;

//-- Panel with zuul jobs from elastic --------------------
local zuulJobList =
  tablePanel.new('List of zuul jobs')
  + tablePanel.panelOptions.withDescription('Description')
  + tablePanel.queryOptions.withDatasource(datasource)
  + tablePanel.queryOptions.withTargets(zuulOpensearchTarget(datasource, queryElastic))
  + tablePanel.queryOptions.withInterval('1m')
  + tablePanel.queryOptions.withTimeFrom('12h')
  + tablePanel.queryOptions.withTransformations([
      selectColumns,
      epochToMs("start_time", "Start Time"),
      epochToMs("end_time", "End Time"),
      reorderColumns,
      renameColumns,
      excludeEpochTimeColumns,
    ])
  + tablePanel.options.withShowHeader(true)
  + tablePanel.options.footer.TableFooterOptions.withEnablePagination(true)
  + tablePanel.standardOptions.withMappings([
      mapNameColors('SUCCESS', 'dark-green'),
      mapNameColors('FAILURE', 'dark-red'),
      mapNameColors('POST_FAILURE', 'dark-red'),
      mapNameColors('ERROR', 'dark-red'),
      mapNameColors('RETRY_LIMIT', 'dark-yellow'),
      mapNameColors('.*', 'text'),
    ])
  + tablePanel.standardOptions.withOverrides([
      colWidth('Job', 250),
      colWidth('Status', 90),
      colWidth('Start Time', 155),
      colWidth('End Time', 155),
      colWidth('Branch', 105),
      colUnit('Start Time', 'dateTimeAsIso'),
      colUnit('End Time', 'dateTimeAsIso'),
      colUrl('Log URL'),
    ])
  + tablePanel.fieldConfig.defaults.withCustom(setCustom(true, 'color-text'))
  + tablePanel.gridPos.withH(16)
  + tablePanel.gridPos.withW(23)
  + tablePanel.gridPos.withX(0)
  + tablePanel.gridPos.withY(0)
;

//-- dashboard with zull job panel ------------------------
dashboard.new('Zuul jobs list')
  + dashboard.withDescription('List of zuul jobs')
  + dashboard.withTags('zuul-jobs')
  + dashboard.withLinks([linkOverview])
  + dashboard.withPanels([
      zuulJobList,
    ])
  + dashboardUid
