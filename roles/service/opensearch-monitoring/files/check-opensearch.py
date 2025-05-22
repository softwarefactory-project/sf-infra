#!/usr/bin/env python3

import argparse
import json
import pytz
import requests
import os
from dateutil import parser as date_parser


def get_arguments():
    parser = argparse.ArgumentParser(description='Check ES state')
    parser.add_argument('--opensearch-url',
                        required=True,
                        help='OpenSearch URL',
                        default='https://localhost:9200')
    parser.add_argument('--user', help='Username for login to the ES')
    parser.add_argument('--password', help='User password for login to the ES')
    parser.add_argument('--index', help='Index to check', default='logstash-*')
    parser.add_argument('--key', help='Use cert key for auth. Requires:'
                                      '--cert and --ca-cert param')
    parser.add_argument('--cert', help='Use cert for auth. Requires:'
                                       '--key and --ca-cert')
    parser.add_argument('--ca-cert', help='Use ca-cert for auth. Requires:'
                                          '--key and --cert')
    parser.add_argument('--collector_path',
                        required=False,
                        default='/var/lib/node_exporter/textfile_collector/'
                        'metrics.prom',
                        help='Path for writing stats for node_exporter')
    parser.add_argument('--insecure', action='store_true')
    parser.add_argument('--query-delta',
                        default=3,
                        help='How many days from last log is needed')
    args = parser.parse_args()
    return args


def do_query(es_url, user, password, index, insecure, cert, key, ca_cert):
    if not es_url.endswith('/') or not es_url.endswith('*'):
        es_url = "%s/%s/_search" % (es_url, index)

    headers = {
        'Content-Type': 'application/json'
    }

    data = {
        "query": {
            "match_all": {}
        },
        "sort": [{
            "@timestamp": {
                "order": "desc"
            }
        }],
        "size": "1"
    }
    kwargs = {}
    if user and password:
        kwargs['http_auth'] = (user, password)
    if key and cert:
        kwargs['cert'] = (cert, key)
    if ca_cert:
        kwargs['verify'] = ca_cert

    if insecure:
        kwargs['verify'] = False

    kwargs['data'] = json.dumps(data)
    kwargs['headers'] = headers

    response = requests.get(es_url, **kwargs)
    es_search = response.json()
    if not es_search:
        return
    es_source = es_search['hits']['hits'][0]['_source']
    return es_source.get('@timestamp') if es_source.get(
        '@timestamp') else es_source.get('timestamp')


def convert_to_timestamp(query_date):
    return date_parser.parse(query_date).timestamp()


def convert_dict_to_string(metrics):
    return "\n".join(["%s %s" % (k, v) for (k, v) in metrics.items()])


def remove_collector_file(collector_path):
    try:
        os.remove(collector_path)
    except OSError:
        pass
    except Exception:
        raise


def write_metrics_to_collector(collector_path, metrics):
    with open(collector_path, 'w') as f:
        f.write(metrics + "\n" + " ")


if __name__ == '__main__':
    args = get_arguments()
    timezone = pytz.UTC
    remove_collector_file(args.collector_path)
    query_date = do_query(args.opensearch_url, args.user, args.password,
                          args.index, args.insecure, args.cert, args.key,
                          args.ca_cert)
    query_timestamp = convert_to_timestamp(query_date)
    metrics = {
        'opensearch_last_update{index="%s"}' % args.index: query_timestamp
    }
    metrics = convert_dict_to_string(metrics)
    write_metrics_to_collector(args.collector_path, metrics)
