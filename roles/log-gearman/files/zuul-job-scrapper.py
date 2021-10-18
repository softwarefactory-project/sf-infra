#!/usr/bin/env python3
# Copyright (C) 2021 Red Hat
# SPDX-License-Identifier: AGPL-3.0-or-later

"""
The goal is to push recent zuul builds into log gearman processor.

[ CLI ] -> [ Config ] -> [ ZuulFetcher ] -> [ LogPublisher ]
"""


import multiprocessing
import argparse
import gear
import datetime
import json
import logging
import requests
import sys
import time
import urllib
import yaml

from distutils.version import StrictVersion as s_version

file_to_check = [
    "job-output.txt.gz",
    "job-output.txt",
    "postci.txt",
    "postci.txt.gz",
    "var/log/extra/logstash.txt",
    "var/log/extra/logstash.txt.gz",
    "var/log/extra/errors.txt",
    "var/log/extra/errors.txt.gz",
]

config = """
files:
  - name: job-output.txt
    tags:
      - console
      - console.html
  # tripleo logs
  - name: postci.txt
    tags:
      - console
      - postci
  - name: var/log/extra/logstash.txt
    tags:
      - console
      - postci
  - name: var/log/extra/errors.txt
    tags:
      - console
      - errors
"""

###############################################################################
# CLI
###############################################################################


def get_arguments():
    parser = argparse.ArgumentParser(
        description="Fetch and push last Zuul CI job logs into gearman."
    )
    parser.add_argument("--zuul-api-url", help="URL for Zuul API",
                        required=True)
    parser.add_argument(
        "--job-name",
        help="CI job name. If not set it would scrap " "every latest builds",
    )
    parser.add_argument("--gearman-server", help="Gearman host addresss")
    parser.add_argument(
        "--gearman-port",
        help="Gearman listen port. " "Defaults to 4731.",
        default=4731,
    )
    parser.add_argument("--follow", help="Keep polling zuul builds",
                        action="store_true")
    parser.add_argument(
        "--insecure", action="store_false", help="Skip validating SSL cert"
    )
    parser.add_argument(
        "--checkpoint-file",
        help="File that will keep" "information about last uuid timestamp for "
             " a job.",
    )
    parser.add_argument(
        "--ignore-checkpoint",
        help="Ignore last job uuid " "that is set in --checkpoint-file",
        action="store_true",
    )
    parser.add_argument("--debug", action="store_true",
                        help="Print more informations")
    args = parser.parse_args()
    return args


###############################################################################
# Configuration of this process
###############################################################################


def parse_time(s):
    return datetime.datetime.strptime(s, "%Y-%m-%dT%H:%M:%S")


def format_time(d):
    return datetime.datetime.strftime(d, "%Y-%m-%dT%H:%M:%S")


class Config:
    def __init__(self, args):
        self.checkpoint = None
        self.filename = args.checkpoint_file
        if args.checkpoint_file and args.job_name:
            self.filename += args.job_name
        if args.checkpoint_file and not args.ignore_checkpoint:
            try:
                self.checkpoint = \
                    parse_time(open(self.filename).read().strip())
            except Exception:
                logging.exception("Can't load the checkpoint")
        if not self.checkpoint:
            self.checkpoint = (datetime.datetime.now() -
                               datetime.timedelta(days=1))

        url_path = args.zuul_api_url.split("/")
        if url_path[-3] != "api" and url_path[-2] != "tenant":
            print(
                "ERROR: zuul-api-url needs to be in the form "
                "of: https://<fqdn>/api/tenant/<tenant-name>"
            )
            sys.exit(1)
        self.tenant = url_path[-1]

    def save(self, start_time):
        if self.filename:
            open(self.filename, "w").write(format_time(start_time))
        self.checkpoint = start_time

    def is_recent(self, build):
        if parse_time(build["end_time"]) > self.checkpoint:
            return True


###############################################################################
# Fetch zuul builds
###############################################################################
def _zuul_complete_available(zuul_url, insecure):
    url = zuul_url + '/status'
    zuul_status = requests.get(url, verify=insecure)
    zuul_status.raise_for_status()
    zuul_version = zuul_status.json().get('zuul_version')
    if zuul_version and (
            s_version(zuul_version.split('-')[0]) >= s_version('4.7.0')):
        return '&complete=true'


def get_last_job_results(zuul_url, job_name, insecure):
    extra = ("&job_name=" + job_name) if job_name else ""
    pos, size = 0, 100
    zuul_url = zuul_url.rstrip("/")
    zuul_complete = _zuul_complete_available(zuul_url, insecure)
    if zuul_complete:
        extra = extra + zuul_complete
    base_url = zuul_url + "/builds?limit=" + str(size) + extra

    while True:
        url = base_url + "&skip=" + str(pos)
        logging.info("Getting job results %s", url)
        jobs_result = requests.get(url, verify=insecure)
        jobs_result.raise_for_status()
        for job in jobs_result.json():
            yield job
        pos += size


###############################################################################
# Log publisher
###############################################################################
def check_specified_files(job_result):
    available_files = []
    for f in file_to_check:
        if not job_result["log_url"]:
            continue
        response = requests.get("%s%s" % (job_result["log_url"], f))
        if response.status_code == 200:
            available_files.append(f)
    return available_files


class LogMatcher(object):
    def __init__(self, server, port, success, log_url, host_vars):
        self.client = gear.Client()
        self.client.addServer(server, port)
        self.hosts = host_vars
        self.success = success
        self.log_url = log_url

    def submitJobs(self, jobname, files, result):
        self.client.waitForServer(90)
        ret = []
        for f in files:
            output = self.makeOutput(f, result)
            output = json.dumps(output).encode("utf8")
            job = gear.TextJob(jobname, output)
            self.client.submitJob(job, background=True)
            ret.append(dict(handle=job.handle, arguments=output))
        return ret

    def makeOutput(self, file_object, result):
        output = {}
        output["retry"] = False
        output["event"] = self.makeEvent(file_object, result)
        output["source_url"] = output["event"]["fields"]["log_url"]
        return output

    def makeEvent(self, file_object, result):
        out_event = {}
        tags = []
        out_event["fields"] = self.makeFields(file_object, result)
        config_files = yaml.safe_load(config)
        for f in config_files["files"]:
            if file_object in f["name"] or \
                    file_object.replace(".gz", "") in f["name"]:
                tags = f["tags"]
                break

        out_event["tags"] = [file_object] + tags
        return out_event

    def makeFields(self, filename, result):
        fields = {}
        fields["build_node"] = "zuul-executor"
        fields["filename"] = filename
        fields["build_name"] = result["job_name"]
        fields["build_status"] = (
            "SUCCESS" if result["result"] == "SUCCESS" else "FAILURE"
        )
        fields["project"] = result["project"]
        fields["voting"] = int(result["voting"])
        fields["build_set"] = result["buildset"]
        fields["build_queue"] = result["pipeline"]
        fields["build_ref"] = result["ref"]
        fields["build_branch"] = result.get("branch", "UNKNOWN")
        fields["build_zuul_url"] = "N/A"

        if "change" in result:
            fields["build_change"] = result["change"]
            fields["build_patchset"] = result["patchset"]
        elif "newrev" in result:
            fields["build_newrev"] = result.get("newrev", "UNKNOWN")

        fields["node_provider"] = "local"
        log_url = urllib.parse.urljoin(result["log_url"], filename)
        fields["log_url"] = log_url
        fields["tenant"] = result["tenant"]

        if "executor" in result and "hostname" in result["executor"]:
            fields["zuul_executor"] = result["executor"]["hostname"]

        fields["build_uuid"] = result["buildset"]["uuid"]

        return fields


def setup_logging(debug):
    if debug:
        logging.basicConfig(format="%(asctime)s %(message)s",
                            level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.INFO)
    logging.debug("Zuul Job Scrapper is starting...")


def run_build(build):
    logging.info(
        "Processing logs for %s | %s | %s | %s",
        build["job_name"],
        build["end_time"],
        build["result"],
        build["uuid"],
    )

    results = dict(files=[], jobs=[], invocation={})

    lmc = LogMatcher(
        args.gearman_server,
        args.gearman_port,
        build["result"],
        build["log_url"],
        {},
    )
    results["files"] = check_specified_files(build)

    lmc.submitJobs("push-log", results["files"], build)


def run(args):
    start_time = datetime.datetime.now()
    config = Config(args)

    builds = []
    for build in get_last_job_results(args.zuul_api_url, args.job_name,
                                      args.insecure):
        if not config.is_recent(build):
            break
        # add missing informations
        build["tenant"] = config.tenant
        builds.append(build)

    logging.info("Processing %d builds", len(builds))

    if args.job_name:
        builds = list(filter(lambda x: x["job_name"] == args.job_name, builds))

    try:
        pool = multiprocessing.Pool()
        pool.map(run_build, builds)
    finally:
        config.save(start_time)


def main(args):
    while True:
        run(args)
        if not args.follow:
            break
        time.sleep(120)


if __name__ == "__main__":
    args = get_arguments()
    setup_logging(args.debug)
    main(args)
