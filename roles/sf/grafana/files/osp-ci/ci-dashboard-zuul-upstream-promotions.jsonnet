{
   "description": "List of Zuul Jobs under different upstream Openstack releases promotion pipelines",
   "panels": [
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-periodic-container-master-centos9 jobs",
         "fieldConfig": {
            "defaults": {
               "custom": {
                  "displayMode": "color-text",
                  "filterable": true
               },
               "mappings": [
                  {
                     "options": {
                        "pattern": "SUCCESS",
                        "result": {
                           "color": "dark-green"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "POST_FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "ERROR",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "NODE_FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "RETRY_LIMIT",
                        "result": {
                           "color": "dark-yellow"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": ".*",
                        "result": {
                           "color": "text"
                        }
                     },
                     "type": "regex"
                  }
               ],
               "thresholds": {
                  "mode": "absolute",
                  "steps": [
                     {
                        "color": "text"
                     },
                     {
                        "value": null
                     }
                  ]
               }
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Job"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 250
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Status"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 90
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Start Time"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 155
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "End Time"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 155
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Branch"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 105
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Start Time"
                  },
                  "properties": [
                     {
                        "id": "unit",
                        "value": "dateTimeAsIso"
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "End Time"
                  },
                  "properties": [
                     {
                        "id": "unit",
                        "value": "dateTimeAsIso"
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Log URL"
                  },
                  "properties": [
                     {
                        "id": "links",
                        "value": [
                           {
                              "targetBlank": true,
                              "title": "",
                              "url": "${__value.text}"
                           }
                        ]
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 10,
            "w": 24,
            "x": 0,
            "y": 0
         },
         "id": 1,
         "interval": "1m",
         "options": {
            "footer": {
               "enablePagination": true
            },
            "showHeader": true
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-periodic-container-master-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-periodic-container-master-centos9 jobs zuul jobs",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
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
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "End Time": 4,
                     "Start Time": 2,
                     "branch": 7,
                     "end_time": 5,
                     "job_name": 0,
                     "log_url": 6,
                     "message": 8,
                     "project": 9,
                     "result": 1,
                     "start_time": 3,
                     "tenant": 10
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "branch": "Branch",
                     "end_time": "",
                     "job_name": "Job",
                     "log_url": "Log URL",
                     "message": "Message",
                     "project": "Project",
                     "result": "Status",
                     "start_time": "",
                     "tenant": "Tenant"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "excludeByName": {
                     "end_time": true,
                     "start_time": true
                  }
               }
            }
         ],
         "type": "table"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-promote-component jobs chart percents",
         "fieldConfig": {
            "defaults": {
               "filterable": true
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "SUCCESS"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "green",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-red",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "POST_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "ERROR"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "NODE_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "RETRY_LIMIT"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-yellow",
                           "mode": "fixed"
                        }
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 5,
            "w": 12,
            "x": 0,
            "y": 10
         },
         "id": 2,
         "interval": "1m",
         "options": {
            "legend": {
               "displayMode": "list",
               "placement": "right",
               "values": [ ]
            },
            "pieType": "pie",
            "reduceOptions": {
               "calcs": [
                  "lastNotNull"
               ],
               "fields": "",
               "values": false
            },
            "tooltip": {
               "mode": "single"
            }
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-periodic-container-master-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-periodic-container-master-centos9 jobs Chart",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
                        "job_name",
                        "result"
                     ]
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "job_name": 0,
                     "result": 1
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "job_name": "Job",
                     "result": "Status"
                  }
               }
            },
            {
               "id": "groupBy",
               "options": {
                  "fields": {
                     "Job": {
                        "aggregations": [
                           "count"
                        ],
                        "operation": "aggregate"
                     },
                     "Status": {
                        "aggregations": [ ],
                        "operation": "groupby"
                     }
                  }
               }
            },
            {
               "id": "rowsToFields",
               "options": { }
            }
         ],
         "type": "piechart"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-periodic-container-master-centos9 jobs gauge",
         "fieldConfig": {
            "defaults": {
               "filterable": true
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "SUCCESS"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "green",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-red",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "POST_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "ERROR"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "NODE_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "RETRY_LIMIT"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-yellow",
                           "mode": "fixed"
                        }
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 5,
            "w": 12,
            "x": 12,
            "y": 10
         },
         "id": 3,
         "interval": "1m",
         "options": {
            "orientation": "auto",
            "reduceOptions": {
               "calcs": [
                  "lastNotNull"
               ],
               "fields": "",
               "values": false
            }
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-periodic-container-master-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-periodic-container-master-centos9 jobs Gauge",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
                        "job_name",
                        "result"
                     ]
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "job_name": 0,
                     "result": 1
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "job_name": "Job",
                     "result": "Status"
                  }
               }
            },
            {
               "id": "groupBy",
               "options": {
                  "fields": {
                     "Job": {
                        "aggregations": [
                           "count"
                        ],
                        "operation": "aggregate"
                     },
                     "Status": {
                        "aggregations": [ ],
                        "operation": "groupby"
                     }
                  }
               }
            },
            {
               "id": "rowsToFields",
               "options": { }
            }
         ],
         "type": "gauge"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-periodic-container-antelope-centos9 jobs",
         "fieldConfig": {
            "defaults": {
               "custom": {
                  "displayMode": "color-text",
                  "filterable": true
               },
               "mappings": [
                  {
                     "options": {
                        "pattern": "SUCCESS",
                        "result": {
                           "color": "dark-green"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "POST_FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "ERROR",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "NODE_FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "RETRY_LIMIT",
                        "result": {
                           "color": "dark-yellow"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": ".*",
                        "result": {
                           "color": "text"
                        }
                     },
                     "type": "regex"
                  }
               ],
               "thresholds": {
                  "mode": "absolute",
                  "steps": [
                     {
                        "color": "text"
                     },
                     {
                        "value": null
                     }
                  ]
               }
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Job"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 250
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Status"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 90
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Start Time"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 155
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "End Time"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 155
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Branch"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 105
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Start Time"
                  },
                  "properties": [
                     {
                        "id": "unit",
                        "value": "dateTimeAsIso"
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "End Time"
                  },
                  "properties": [
                     {
                        "id": "unit",
                        "value": "dateTimeAsIso"
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Log URL"
                  },
                  "properties": [
                     {
                        "id": "links",
                        "value": [
                           {
                              "targetBlank": true,
                              "title": "",
                              "url": "${__value.text}"
                           }
                        ]
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 10,
            "w": 24,
            "x": 0,
            "y": 15
         },
         "id": 4,
         "interval": "1m",
         "options": {
            "footer": {
               "enablePagination": true
            },
            "showHeader": true
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-periodic-container-antelope-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-periodic-container-antelope-centos9 jobs zuul jobs",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
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
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "End Time": 4,
                     "Start Time": 2,
                     "branch": 7,
                     "end_time": 5,
                     "job_name": 0,
                     "log_url": 6,
                     "message": 8,
                     "project": 9,
                     "result": 1,
                     "start_time": 3,
                     "tenant": 10
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "branch": "Branch",
                     "end_time": "",
                     "job_name": "Job",
                     "log_url": "Log URL",
                     "message": "Message",
                     "project": "Project",
                     "result": "Status",
                     "start_time": "",
                     "tenant": "Tenant"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "excludeByName": {
                     "end_time": true,
                     "start_time": true
                  }
               }
            }
         ],
         "type": "table"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-promote-component jobs chart percents",
         "fieldConfig": {
            "defaults": {
               "filterable": true
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "SUCCESS"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "green",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-red",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "POST_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "ERROR"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "NODE_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "RETRY_LIMIT"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-yellow",
                           "mode": "fixed"
                        }
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 5,
            "w": 12,
            "x": 0,
            "y": 25
         },
         "id": 5,
         "interval": "1m",
         "options": {
            "legend": {
               "displayMode": "list",
               "placement": "right",
               "values": [ ]
            },
            "pieType": "pie",
            "reduceOptions": {
               "calcs": [
                  "lastNotNull"
               ],
               "fields": "",
               "values": false
            },
            "tooltip": {
               "mode": "single"
            }
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-periodic-container-antelope-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-periodic-container-antelope-centos9 jobs Chart",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
                        "job_name",
                        "result"
                     ]
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "job_name": 0,
                     "result": 1
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "job_name": "Job",
                     "result": "Status"
                  }
               }
            },
            {
               "id": "groupBy",
               "options": {
                  "fields": {
                     "Job": {
                        "aggregations": [
                           "count"
                        ],
                        "operation": "aggregate"
                     },
                     "Status": {
                        "aggregations": [ ],
                        "operation": "groupby"
                     }
                  }
               }
            },
            {
               "id": "rowsToFields",
               "options": { }
            }
         ],
         "type": "piechart"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-periodic-container-antelope-centos9 jobs gauge",
         "fieldConfig": {
            "defaults": {
               "filterable": true
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "SUCCESS"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "green",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-red",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "POST_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "ERROR"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "NODE_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "RETRY_LIMIT"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-yellow",
                           "mode": "fixed"
                        }
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 5,
            "w": 12,
            "x": 12,
            "y": 25
         },
         "id": 6,
         "interval": "1m",
         "options": {
            "orientation": "auto",
            "reduceOptions": {
               "calcs": [
                  "lastNotNull"
               ],
               "fields": "",
               "values": false
            }
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-periodic-container-antelope-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-periodic-container-antelope-centos9 jobs Gauge",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
                        "job_name",
                        "result"
                     ]
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "job_name": 0,
                     "result": 1
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "job_name": "Job",
                     "result": "Status"
                  }
               }
            },
            {
               "id": "groupBy",
               "options": {
                  "fields": {
                     "Job": {
                        "aggregations": [
                           "count"
                        ],
                        "operation": "aggregate"
                     },
                     "Status": {
                        "aggregations": [ ],
                        "operation": "groupby"
                     }
                  }
               }
            },
            {
               "id": "rowsToFields",
               "options": { }
            }
         ],
         "type": "gauge"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-operators-periodic-integration-antelope-centos9 jobs",
         "fieldConfig": {
            "defaults": {
               "custom": {
                  "displayMode": "color-text",
                  "filterable": true
               },
               "mappings": [
                  {
                     "options": {
                        "pattern": "SUCCESS",
                        "result": {
                           "color": "dark-green"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "POST_FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "ERROR",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "NODE_FAILURE",
                        "result": {
                           "color": "dark-red"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": "RETRY_LIMIT",
                        "result": {
                           "color": "dark-yellow"
                        }
                     },
                     "type": "regex"
                  },
                  {
                     "options": {
                        "pattern": ".*",
                        "result": {
                           "color": "text"
                        }
                     },
                     "type": "regex"
                  }
               ],
               "thresholds": {
                  "mode": "absolute",
                  "steps": [
                     {
                        "color": "text"
                     },
                     {
                        "value": null
                     }
                  ]
               }
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Job"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 250
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Status"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 90
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Start Time"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 155
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "End Time"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 155
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Branch"
                  },
                  "properties": [
                     {
                        "id": "custom.width",
                        "value": 105
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Start Time"
                  },
                  "properties": [
                     {
                        "id": "unit",
                        "value": "dateTimeAsIso"
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "End Time"
                  },
                  "properties": [
                     {
                        "id": "unit",
                        "value": "dateTimeAsIso"
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "Log URL"
                  },
                  "properties": [
                     {
                        "id": "links",
                        "value": [
                           {
                              "targetBlank": true,
                              "title": "",
                              "url": "${__value.text}"
                           }
                        ]
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 10,
            "w": 24,
            "x": 0,
            "y": 30
         },
         "id": 7,
         "interval": "1m",
         "options": {
            "footer": {
               "enablePagination": true
            },
            "showHeader": true
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-operators-periodic-integration-antelope-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-operators-periodic-integration-antelope-centos9 jobs zuul jobs",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
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
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "End Time": 4,
                     "Start Time": 2,
                     "branch": 7,
                     "end_time": 5,
                     "job_name": 0,
                     "log_url": 6,
                     "message": 8,
                     "project": 9,
                     "result": 1,
                     "start_time": 3,
                     "tenant": 10
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "branch": "Branch",
                     "end_time": "",
                     "job_name": "Job",
                     "log_url": "Log URL",
                     "message": "Message",
                     "project": "Project",
                     "result": "Status",
                     "start_time": "",
                     "tenant": "Tenant"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "excludeByName": {
                     "end_time": true,
                     "start_time": true
                  }
               }
            }
         ],
         "type": "table"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-promote-component jobs chart percents",
         "fieldConfig": {
            "defaults": {
               "filterable": true
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "SUCCESS"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "green",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-red",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "POST_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "ERROR"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "NODE_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "RETRY_LIMIT"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-yellow",
                           "mode": "fixed"
                        }
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 5,
            "w": 12,
            "x": 0,
            "y": 40
         },
         "id": 8,
         "interval": "1m",
         "options": {
            "legend": {
               "displayMode": "list",
               "placement": "right",
               "values": [ ]
            },
            "pieType": "pie",
            "reduceOptions": {
               "calcs": [
                  "lastNotNull"
               ],
               "fields": "",
               "values": false
            },
            "tooltip": {
               "mode": "single"
            }
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-operators-periodic-integration-antelope-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-operators-periodic-integration-antelope-centos9 jobs Chart",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
                        "job_name",
                        "result"
                     ]
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "job_name": 0,
                     "result": 1
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "job_name": "Job",
                     "result": "Status"
                  }
               }
            },
            {
               "id": "groupBy",
               "options": {
                  "fields": {
                     "Job": {
                        "aggregations": [
                           "count"
                        ],
                        "operation": "aggregate"
                     },
                     "Status": {
                        "aggregations": [ ],
                        "operation": "groupby"
                     }
                  }
               }
            },
            {
               "id": "rowsToFields",
               "options": { }
            }
         ],
         "type": "piechart"
      },
      {
         "datasource": {
            "type": "lucene",
            "uid": "opensearch-rdoproject-zuul"
         },
         "description": "Jobs under the Pipeline: openstack-operators-periodic-integration-antelope-centos9 jobs gauge",
         "fieldConfig": {
            "defaults": {
               "filterable": true
            },
            "overrides": [
               {
                  "matcher": {
                     "id": "byName",
                     "options": "SUCCESS"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "green",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-red",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "POST_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "ERROR"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "NODE_FAILURE"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "#770f1b",
                           "mode": "fixed"
                        }
                     }
                  ]
               },
               {
                  "matcher": {
                     "id": "byName",
                     "options": "RETRY_LIMIT"
                  },
                  "properties": [
                     {
                        "id": "color",
                        "value": {
                           "fixedColor": "dark-yellow",
                           "mode": "fixed"
                        }
                     }
                  ]
               }
            ]
         },
         "gridPos": {
            "h": 5,
            "w": 12,
            "x": 12,
            "y": 40
         },
         "id": 9,
         "interval": "1m",
         "options": {
            "orientation": "auto",
            "reduceOptions": {
               "calcs": [
                  "lastNotNull"
               ],
               "fields": "",
               "values": false
            }
         },
         "pluginVersion": "v10.4.0",
         "targets": [
            {
               "bucketAggs": [ ],
               "datasource": "opensearch-rdoproject-zuul",
               "metrics": [
                  {
                     "settings": {
                        "order": "desc",
                        "size": "500",
                        "useTimeRange": true
                     },
                     "type": "raw_data"
                  }
               ],
               "query": "pipeline.keyword: \"openstack-operators-periodic-integration-antelope-centos9\" AND build_type: \"build\"",
               "queryType": "lucene",
               "timeField": "@timestamp"
            }
         ],
         "title": "Pipeline: openstack-operators-periodic-integration-antelope-centos9 jobs Gauge",
         "transformations": [
            {
               "id": "filterFieldsByName",
               "options": {
                  "include": {
                     "names": [
                        "job_name",
                        "result"
                     ]
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "Start Time",
                  "binary": {
                     "left": "start_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "calculateField",
               "options": {
                  "alias": "End Time",
                  "binary": {
                     "left": "end_time",
                     "operator": "*",
                     "reducer": "sum",
                     "right": "1000"
                  },
                  "mode": "binary",
                  "reduce": {
                     "reducer": "sum"
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "indexByName": {
                     "job_name": 0,
                     "result": 1
                  }
               }
            },
            {
               "id": "organize",
               "options": {
                  "renameByName": {
                     "job_name": "Job",
                     "result": "Status"
                  }
               }
            },
            {
               "id": "groupBy",
               "options": {
                  "fields": {
                     "Job": {
                        "aggregations": [
                           "count"
                        ],
                        "operation": "aggregate"
                     },
                     "Status": {
                        "aggregations": [ ],
                        "operation": "groupby"
                     }
                  }
               }
            },
            {
               "id": "rowsToFields",
               "options": { }
            }
         ],
         "type": "gauge"
      }
   ],
   "schemaVersion": 36,
   "tags": [
      "zuul-jobs-upstream-promotions"
   ],
   "time": {
      "from": "now-24h",
      "to": "now"
   },
   "timezone": "utc",
   "title": "Openstack Upstream Releases Promotions",
   "uid": "7ff52dc9be88f4f31afce921e6556212"
}
