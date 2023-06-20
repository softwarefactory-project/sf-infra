#!/usr/bin/env python3
import argparse
import glob
import os
import re


def get_arguments():
    parser = argparse.ArgumentParser(description='Check state for DLRN worker')
    parser.add_argument('--collector_path',
                        required=False,
                        default='/var/lib/node_exporter/textfile_collector/'
                        'metrics.prom',
                        help='Path for writing stats for node_exporter')
    parser.add_argument('--workers',
                        required=True,
                        help='List of Workers to be checked for DLRN status')
    args = parser.parse_args()
    return args


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
    with open(collector_path, 'a') as f:
        f.write(metrics + "\n" + " ")


def get_mtime_last_logfile(worker):
    latest_file = None
    folder_path = "/home/{}/dlrn-logs".format(worker)
    files = [f for f in glob.iglob(
        folder_path + "/dlrn-run.*.log")
        if re.search(r"dlrn-run.[0-9]+.log", f)]
    if bool(len(files)):
        latest_file = max(files, key=os.path.getmtime)
        return round(os.path.getmtime(latest_file))
    raise FileNotFoundError("No logs files for given worker")


if __name__ == '__main__':
    args = get_arguments()
    remove_collector_file(args.collector_path)
    if bool(len(args.workers)):
        for worker in args.workers.strip('][').split(', '):
            try:
                mtime = get_mtime_last_logfile(worker)
            except FileNotFoundError:
                continue
            metrics = {
                'dlrn_last_update{index="%s"}' % worker: mtime
            }
            metrics = convert_dict_to_string(metrics)
            write_metrics_to_collector(args.collector_path, metrics)
