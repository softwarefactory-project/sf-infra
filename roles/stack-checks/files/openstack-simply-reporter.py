#!/usr/bin/env python3

import argparse
import openstack
import os


STACK_REPORT_STATE = ['create_complete', 'delete_failed',
                      'delete_in_progress']


def get_arguments():
    parser = argparse.ArgumentParser(description='The simply Openstack '
                                                 'exporter for Prometheus')
    parser.add_argument('--os-cloud', required=False, action='append',
                        default=[], help='Name of the cloud to '
                        'get information')
    parser.add_argument('--os-clouds', required=False, nargs="+", default=[],
                        help='Name of the cloud to get information')
    parser.add_argument('--collector_path', required=False,
                        default='/var/lib/node_exporter/textfile_collector/'
                                'metrics.prom',
                        help='Path for writing stats for node_exporter')
    parser.add_argument('--stack-status', action='store_true',
                        help='Write stack statuses into the collector')

    args = parser.parse_args()

    return args


def remove_collector_file(collector_path):
    try:
        os.remove(collector_path)
    except FileNotFoundError:
        pass
    except Exception:
        raise


def count_metric(metric_name, metrics):
    metrics[metric_name] = metrics.setdefault(metric_name, 0) + 1


def convert_dict_to_string(metrics):
    return "\n".join(["%s %s" % (k, v) for (k, v) in metrics.items()])


def write_metrics_to_collector(collector_path, metrics):
    with open(collector_path, 'w') as f:
        f.write(metrics)


def get_stack_status(cloud, metrics):
    base_metric_name = "stack"

    for stack in cloud.list_stacks():
        if stack.stack_status.lower() in STACK_REPORT_STATE:
            metric_name = "%s_%s{cloud=%s}" % (base_metric_name,
                                               stack.stack_status.lower(),
                                               stack.location.cloud)
            count_metric(metric_name, metrics)

    return metrics


if __name__ == '__main__':
    metrics = {}
    args = get_arguments()

    if not args.os_cloud and not args.os_clouds:
        raise("Please set --os-cloud or --os-clouds param!")

    clouds = args.os_cloud + args.os_clouds
    remove_collector_file(args.collector_path)
    for os_cloud in set(clouds):
        cloud = openstack.connect(cloud=os_cloud)
        metrics = get_stack_status(cloud, metrics)

    metrics = convert_dict_to_string(metrics)
    write_metrics_to_collector(args.collector_path, metrics)
