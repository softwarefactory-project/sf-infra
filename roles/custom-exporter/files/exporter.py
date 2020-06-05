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
import time
import threading
import urllib.parse
from functools import reduce

from typing import Any, Callable, List
# Need python3-systemd and python3-prometheus_client
from systemd import journal as systemd_journal  # type: ignore
from prometheus_client import start_http_server, Counter, Gauge  # type: ignore
from zuul_stats_client import utils as zuul_stats_client  # type: ignore

traceback = Counter('traceback_total', 'Number of tracebacks', ['unit'])
segfault = Counter('segfault_total', 'Number of segfault', ['unit'])
TRACE_COOKIE = 'Traceback (most recent call last):'


def usage() -> argparse.Namespace:
    p = argparse.ArgumentParser()
    p.add_argument("--port", help="Prometheus target port",
                   type=int, default=9101)
    p.add_argument(
        "--journald", action="store_true", help="Activate journald collector")
    p.add_argument(
        "--journald-config", metavar="DIR", help="Add custom event metrics")
    p.add_argument(
        "--zuul", metavar="URL", help="Activate zuul collector")
    return p.parse_args()


def start_prometheus_endpoint(port: int) -> None:
    """ This starts the prometheus http server thread and returns. """
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
    """ A structure used by the journald exporter. """
    unit: str
    message: str
    metric: str

    def __post_init__(self) -> None:
        self.unit_re = re.compile(self.unit)
        self.message_re = re.compile(self.message)
        self.counter = Counter(self.metric, self.message, ['unit'])

    def process(self, unit: str, message: str) -> None:
        if self.unit_re.match(unit) and self.message_re.match(message):
            self.counter.labels(unit).inc()


def config_files(config_dir: str) -> List[str]:
    """ Return the list of absolute path of journald configurations. """
    return list(map(lambda x: os.path.join(config_dir, x),
                    filter(lambda x: x.endswith(".json"),
                           os.listdir(config_dir))))


def load_config(config_dir: str) -> List[CustomEvent]:
    """ Loads the journald configuration files and return a list of events. """
    return [CustomEvent(*config) for config in
            reduce(lambda a, b: a + b,
                   map(json.load,
                       map(open,
                           config_files(config_dir))))]


def config_mtime(config_dir: str) -> float:
    """ Return maximum mtime of configurations. """
    return reduce(max,
                  map(lambda x: os.stat(x).st_mtime,
                      config_files(config_dir)))


def process_event(
        custom_events: List[CustomEvent],
        unit: str,
        pid: str,
        comm: str,
        message: str) -> None:
    """ Process a journald event and update the metrics. """
    if not unit or not message:
        return
    if TRACE_COOKIE in message:
        traceback.labels(unit).inc()
    if "segfault" in message:
        segfault.labels(unit).inc()
    for custom_event in custom_events:
        custom_event.process(unit, message)


def thread_start(
        target: Callable[[Any], None], args: List[str]) -> threading.Thread:
    """ Helper function to start a thread. """
    thread = threading.Thread(target=target, args=args)
    thread.start()
    return thread


def thread_is_dead(thread: threading.Thread) -> bool:
    """ Return true if a thread is dead. """
    return not thread.is_alive()


def main() -> None:
    args = usage()
    start_prometheus_endpoint(args.port)
    threads = []
    if args.journald or args.journald_config:
        threads.append(thread_start(main_journald, [args.journald_config]))
    if args.zuul:
        threads.append(thread_start(main_zuul, [args.zuul]))
    if not threads:
        print("No collector configured!")
        exit(1)
    while True:
        if any(map(thread_is_dead, threads)):
            print("A thread died!")
            break
        time.sleep(1)
    exit(1)


def main_zuul(api: str) -> None:
    blocked_metric = Gauge(
        "zuul_blocked_change_total",
        "Number of zuul changes blocked in queue",
        ['tenant'])
    tenants = list(map(
        lambda tenant: (tenant, urllib.parse.urljoin(
            api.rstrip('/') + '/', "tenant/" + tenant + "/status")),
        zuul_stats_client.get_zuul_tenants(api)))
    while True:
        for tenant, tenant_status_url in tenants:
            tenant_status = zuul_stats_client.get_zuul_status(
                tenant_status_url)
            blocked = zuul_stats_client.find_long_running_jobs(
                tenant_status, 60 * 60 * 4 * 1000  # 4 hours in ms
            )
            blocked_metric.labels(tenant).set(len(blocked))
            time.sleep(1)
        time.sleep(60)


def main_journald(config: str) -> None:
    """ The journald exporter. """
    custom_events = load_config(config) if config else []
    last_update = config_mtime(config) if config else 0.0
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
            if last_update and last_update < config_mtime(config):
                print("Reloading journald configuration")
                custom_events = load_config(config)
                last_update = config_mtime(config)
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
