local grafonnet = import 'grafonnet-v9.4.0/main.libsonnet';
local esQuery = grafonnet.query.elasticsearch;
local tablePanel = grafonnet.panel.table;
local mapping = tablePanel.valueMapping;
local transform = grafonnet.panel.table.transformation;
local override = tablePanel.fieldOverride.byName;

{
  zuulOpensearchTarget(datasource, query):
    esQuery.withQuery(query)
    + esQuery.withDatasource(datasource)
    + esQuery.withMetrics({ type: 'raw_data', settings: { size: '500', order: 'desc', useTimeRange: true } })
    + esQuery.withTimeField('@timestamp')
    + esQuery.withQueryType('lucene')
    + esQuery.withBucketAggs([]),

  mapNameColors(name, color):
    mapping.RegexMap.options.withPattern(name)
    + mapping.RegexMap.options.result.withColor(color)
    + mapping.RegexMap.withType('regex'),

  zuulStatusColorMappingOptions():
    tablePanel.standardOptions.withMappings([
      self.mapNameColors('SUCCESS', 'dark-green'),
      self.mapNameColors('FAILURE', 'dark-red'),
      self.mapNameColors('POST_FAILURE', 'dark-red'),
      self.mapNameColors('ERROR', 'dark-red'),
      self.mapNameColors('NODE_FAILURE', 'dark-red'),
      self.mapNameColors('RETRY_LIMIT', 'dark-yellow'),
      self.mapNameColors('.*', 'text'),
    ]),

  epochToMs(fieldName, alias):
    transform.withId('calculateField')
    + transform.withOptions({
      alias: alias,
      binary: {
        left: fieldName,
        operator: '*',
        reducer: 'sum',
        right: '1000',
      },
      mode: 'binary',
      reduce: {
        reducer: 'sum',
      },
    }),

  colWidth(name, width):
  override.new(name)
  + override.withProperty('custom.width', width),

  colUrl(name, targetblank=true):
  override.new(name)
  + override.withProperty('links', [{targetBlank: targetblank, title: "", url: "${__value.text}"}]),

  colUnit(name, unit):
  override.new(name)
  + override.withProperty('unit', unit),

}
