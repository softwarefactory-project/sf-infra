#!/bin/env python3
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

"""This service collects traceback and segfault count in journald.
"""

import argparse
import dataclasses
import select
import json
import re
import os
from functools import reduce

from typing import List
# Need python3-systemd and python3-prometheus_client
from systemd import journal as systemd_journal
from prometheus_client import start_http_server, Counter


traceback = Counter('traceback_total', 'Number of tracebacks', ['unit'])
segfault = Counter('segfault_total', 'Number of segfault', ['unit'])
TRACE_COOKIE = 'Traceback (most recent call last):'


def usage():
    p = argparse.ArgumentParser()
    p.add_argument("--port", help="Prometheus target port",
                   type=int, default=9101)
    p.add_argument("--config", help="Custom event configuration directory")
    return p.parse_args()


def start_prometheus_endpoint(port):
    # From https://github.com/prometheus/client_python/issues/414
    from prometheus_client import REGISTRY, PROCESS_COLLECTOR, \
        PLATFORM_COLLECTOR
    # Unregister default metrics
    REGISTRY.unregister(PROCESS_COLLECTOR)
    REGISTRY.unregister(PLATFORM_COLLECTOR)
    for i in ("python_gc_objects_collected_total",):
        REGISTRY.unregister(REGISTRY._names_to_collectors[i])
    start_http_server(port)


@dataclasses.dataclass
class CustomEvent:
    unit: str
    message: str
    metric: str

    def __post_init__(self) -> None:
        self.unit_re = re.compile(self.unit)
        self.message_re = re.compile(self.message)
        self.counter = Counter(self.metric, self.message)

    def process(self, unit, message) -> None:
        if self.unit_re.match(unit) and self.message_re.match(message):
            self.counter.labels(unit).inc()


def config_files(config_dir: str) -> List[str]:
    return map(lambda x: os.path.join(config_dir, x),
               filter(lambda x: x.endswith(".json"),
                      os.listdir(config_dir)))


def load_config(config_dir: str) -> List[CustomEvent]:
    # read bottom-up :)
    return [CustomEvent(*config) for config in
            reduce(lambda a, b: a + b,
                   map(json.load,
                       map(open,
                           config_files(config_dir))))]


def config_mtime(config_dir: str) -> float:
    # Return maximum mtime of configurations
    return reduce(max,
                  map(lambda x: os.stat(x).st_mtime,
                      config_files(config_dir)))


def process_event(custom_events, unit, pid, comm, message):
    if TRACE_COOKIE in message:
        traceback.labels(unit).inc()
    if "segfault" in message:
        segfault.labels(unit).inc()
    for custom_event in custom_events:
        custom_event.process(unit, message)


def main():
    args = usage()
    start_prometheus_endpoint(args.port)
    custom_events = load_config(args.config) if args.config else []
    last_update = config_mtime(args.config) if args.config else 0.0
    journal = systemd_journal.Reader()
    journal.log_level(systemd_journal.LOG_INFO)
    journal.this_boot()
    journal.seek_tail()
    journal.get_previous()
    fd = journal.fileno()
    p = select.poll()
    poll_event_mask = journal.get_events()
    p.register(fd, poll_event_mask)

    while p.poll():
        try:
            if journal.process() != systemd_journal.APPEND:
                continue
            if last_update and last_update < config_mtime(args.config):
                print("Reloading configuration")
                custom_events = load_config(args.config)
                last_update = config_mtime(args.config)
            for event in journal:
                unit = event.get('_SYSTEMD_UNIT')
                pid = event.get('_PID')
                comm = event.get('_COMM')
                process_event(custom_events, unit, pid,
                              comm, event.get("MESSAGE"))
        except KeyboardInterrupt:
            exit(0)


if __name__ == "__main__":
    main()
