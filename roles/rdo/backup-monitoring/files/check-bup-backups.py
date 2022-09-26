#!/usr/bin/env python3
# Copyright 2021 Red Hat, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

import json
import argparse
import subprocess
import datetime


def usage():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--container", help="The bup container name")
    parser.add_argument(
        "--config", required=True, help="The bup locations configuration")
    parser.add_argument(
        "--test", action="store_true", help="Test with a fake bup")
    return parser.parse_args()


def quote(name):
    return '"' + name + '"'


def timestamp_to_epoch(ts):
    """convert bup timestamp to epoch float
    >>> timestamp_to_epoch("2021-02-01-070939")
    1612163379.0
    """
    try:
        return datetime.datetime.strptime(ts, "%Y-%m-%d-%H%M%S").timestamp()
    except Exception:
        raise RuntimeError("Could not parse datetime %s" % ts)


def month_subdir():
    """create the current month subdir name
    >>> month_subdir()
    "2021-01"
    """
    return datetime.datetime.strftime(datetime.datetime.now(), "%Y-%m")


def process_read(argv):
    """read process stdout"""
    proc = subprocess.Popen(argv, stdout=subprocess.PIPE)
    stdout, _ = proc.communicate()
    if proc.wait():
        raise RuntimeError("%s: failed" % " ".join(argv))
    return stdout.decode("utf-8")


def fake_bup_ls(argv):
    """mock for bup ls output"""
    print("Faking output of: " + " ".join(argv))
    return "\n".join(
        [
            "l--------- ?/?                  52 1970-01-01 00:00 "
            "2021-01-31-070920 -> ../.commit/1f/f3838271e737da0b3483e917a44",
            "l--------- ?/?                  52 1970-01-01 00:00 "
            "2021-02-01-070939 -> ../.commit/4a/9dd7d674286ffa85e601312b5d3",
            "l--------- ?/?                  52 1970-01-01 00:00 "
            "latest -> ../.commit/4a/9dd7d674286ffa85e601312b5db143255a8c87",
            ""
        ]
    )


def fake_location():
    return [dict(dir="/var/lib/bup", domain="test", month_subdir="1")]


def get_metrics(locations, bup_container_name, pread):
    def get_timestamp(location):
        if bup_container_name:
            prefix = ["podman", "exec", "-it", bup_container_name]
        else:
            prefix = ["sudo"]
        if int(location["month_subdir"]):
            bup_dir = location["dir"] + "/" + month_subdir()
        else:
            bup_dir = location["dir"]

        env = ["env", "PATH=/usr/local/bin:/usr/bin", "BUP_DIR=%s" % bup_dir]
        cmd = ["bup", "ls", "-l", location["domain"]]
        all_timestamps = pread(prefix + env + cmd)
        try:
            return all_timestamps.split("\n")[-3].split()[5]
        except Exception:
            raise RuntimeError(
                "Could not get the latest timestamp from: " + all_timestamps)

    def get_metric(location):
        return "bup_last_backup{dir=%s} %s" % (
            quote(location["dir"]),
            timestamp_to_epoch(get_timestamp(location)),
        )

    return map(get_metric, locations)


def main():
    args = usage()
    if args.test:
        locations = fake_location()
        pread = fake_bup_ls
    else:
        locations = json.load(open(args.config))
        pread = process_read
    metrics = "\n".join(get_metrics(locations, args.container, pread))
    if args.test:
        print(metrics)
    else:
        with open(
            "/var/lib/node_exporter/textfile_collector/bup_backup.prom", "w"
        ) as of:
            of.write(metrics + "\n")


if __name__ == "__main__":
    main()
