#!/usr/bin/env python3

import argparse
import pytz
import os

from elasticsearch import Elasticsearch
from dateutil import parser as date_parser


def get_arguments():
    parser = argparse.ArgumentParser(description='Check ES state')
    parser.add_argument('--elasticsearch-url',
                        required=True,
                        help='Elasticsearch URL',
                        default='https://localhost:9200')
    parser.add_argument('--user', help='Username for login to the ES')
    parser.add_argument('--password', help='User password for login to the ES')
    parser.add_argument('--index', help='Index to check', default='logstash-*')
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


def do_query(es_url, user, password, index, insecure):
    data = '''{
        "query": {
            "match_all": {}
        },
        "sort": [{
            "@timestamp": {
                "order": "desc"
            }
        }],
        "size": "1"
    }'''
    kwargs = {'verify_certs': insecure}
    if user and password:
        kwargs['http_auth'] = (user, password)
    es = Elasticsearch(es_url, **kwargs)

    query_kwargs = {'index': index, 'body': str(data)}
    es_search = es.search(**query_kwargs)
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
    query_date = do_query(args.elasticsearch_url, args.user, args.password,
                          args.index, args.insecure)
    query_timestamp = convert_to_timestamp(query_date)
    metrics = {
        'elasticsearch_last_update{index="%s"}' % args.index: query_timestamp
    }

    metrics = convert_dict_to_string(metrics)
    write_metrics_to_collector(args.collector_path, metrics)
