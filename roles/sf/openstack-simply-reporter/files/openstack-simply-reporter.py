#!/usr/bin/env python3

import argparse
import openstack
import os
import pytz

from datetime import datetime, timedelta
from dateutil import parser

STACK_REPORT_STATE = ['create_complete', 'delete_failed',
                      'delete_in_progress']
PORT_REPORT_STATE = ['down', 'active', 'n/a']
PORT_TIMEDELTA = 3


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
    parser.add_argument('--check-network', action='store_true', default=True,
                        help='Take informations about network usage')
    parser.add_argument("--debug", help="Print more information",
                        action="store_true")

    args = parser.parse_args()

    return args


def _quote(name):
    return '"' + name + '"'


def remove_collector_file(collector_path):
    try:
        os.remove(collector_path)
    except OSError:
        pass
    except Exception:
        raise


def count_metric(metric_name, metrics, counted=None):
    if counted:
        metrics[metric_name] = counted
    else:
        metrics[metric_name] = metrics.setdefault(metric_name, 0) + 1


def convert_dict_to_string(metrics):
    return "\n".join(["%s %s" % (k, v) for (k, v) in metrics.items()])


def write_metrics_to_collector(collector_path, metrics):
    with open(collector_path, 'w') as f:
        # NOTE(dpawlik) An empty line on the end is required, and
        # in 'w' mode just '\n' is not enough.
        f.write(metrics + "\n" + " ")


def _check_port_time(port, timezone):
    port_date = parser.parse(port.updated_at)
    past_date = datetime.now() - timedelta(days=PORT_TIMEDELTA)
    port_date = port_date.replace(tzinfo=timezone)
    past_date = past_date.replace(tzinfo=timezone)
    return port_date < past_date


def get_stack_status(cloud, metrics, debug):
    base_metric_name = "stack"

    stacks = cloud.list_stacks()
    if debug:
        print("Available stacks: %s" % stacks)

    for stack in stacks:
        if stack['stack_status'].lower() in STACK_REPORT_STATE:
            metric_name = "%s_%s{cloud=%s}" % (
                base_metric_name, stack['stack_status'].lower(),
                _quote(stack['location']['cloud']))
            count_metric(metric_name, metrics)

    return metrics


def get_ports_status(cloud, metrics, timezone, debug):
    base_metric_name = "port"

    ports = cloud.list_ports()
    if debug:
        print("Available ports: %s" % ports)
    for port in ports:
        old_port = _check_port_time(port, timezone)
        port_status = port.status.lower().replace('/', '')
        if port.status.lower() in PORT_REPORT_STATE:
            if debug:
                print("Type: %s - id: %s - port state: %s - is old: %s" % (
                    base_metric_name, port.id, port_status, str(old_port)))
            metric_name = "%s_%s{cloud=%s, is_old=%s}" % (
                base_metric_name, port_status, _quote(
                    cloud.name), _quote(str(old_port)))
            count_metric(metric_name, metrics)

    return metrics


def _get_subnet_info(network_info):
    # TBD: here we are able to add more information about subnet.
    # For now it will just return list of subnets.
    return network_info['subnets']


def _do_counting(f_ip, subnets, count):
    if f_ip['subnet_id'] in subnets:
        if not count.get(f_ip['subnet_id']):
            count[f_ip['subnet_id']] = 1
        count[f_ip['subnet_id']] += 1
    return count


def _count_ports_with_subnet(subnets, cloud, network_id, debug):
    count = {}
    if network_id:
        ports = cloud.list_ports(filters={"network_id": network_id})
    else:
        if debug:
            print("No network set, taking all...")
        ports = cloud.list_ports()

    for port in ports:
        for f_ip in port.get('fixed_ips', []):
            count = _do_counting(f_ip, subnets, count)
    return count


def get_network_info(cloud, metrics, debug):
    base_metric_name = "network"
    networks = cloud.list_networks()
    if debug:
        print("Available networks: %s" % networks)
    for network in networks:
        related_subnets = _get_subnet_info(network)
        if not related_subnets:
            continue
        if debug:
            print("Related subnets: %s for network %s" % (related_subnets,
                                                          network['id']))

        count_ports = _count_ports_with_subnet(related_subnets, cloud,
                                               network.get('id'), debug)
        if not count_ports:
            continue

        for s_uuid, s_count in count_ports.items():
            if debug:
                print("Type: %s - id: %s - subnet: %s" % (
                    base_metric_name, network.id, s_uuid))
            metric_name = "%s{cloud=%s, subnet_uuid=%s}" % (
                base_metric_name, _quote(cloud.name), _quote(s_uuid))
            count_metric(metric_name, metrics, counted=s_count)
    return metrics


def get_floating_ips(cloud, metrics):
    base_metric_name = "floating_ip"
    counted_fips = len(cloud.list_floating_ips())
    metric_name = "%s{cloud=%s}" % (base_metric_name, _quote(cloud.name))
    count_metric(metric_name, metrics, counted=counted_fips)

    return metrics


if __name__ == '__main__':
    metrics = {}
    args = get_arguments()
    timezone = pytz.UTC

    if not args.os_cloud and not args.os_clouds:
        raise Exception("Please set --os-cloud or --os-clouds param!")

    clouds = args.os_cloud + args.os_clouds
    remove_collector_file(args.collector_path)
    for os_cloud in set(clouds):
        cloud = openstack.connect(cloud=os_cloud)
        metrics = get_stack_status(cloud, metrics, args.debug)
        if args.check_network:
            metrics = get_ports_status(cloud, metrics, timezone, args.debug)
            metrics = get_network_info(cloud, metrics, args.debug)
            metrics = get_floating_ips(cloud, metrics)

    metrics = convert_dict_to_string(metrics)
    write_metrics_to_collector(args.collector_path, metrics)
