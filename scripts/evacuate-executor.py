#!/bin/env python3
# Copyright (C) 2022 Red Hat
# SPDX-License-Identifier: Apache-2.0
#
# A script to evacuate all the jobs running on a given executor.
# This script is meant to be run before stopping the service:
# - call zuul-executor graceful to make it stop accepting new jobs.
# - query zuul status page to collect all the buildset
#   having a job running on the executor.
# - dequeue and re-enqueue all the affected buildset.
# - stop the executor host.

import argparse
from functools import partial
from sfinfra_lib import run, get_tenants


def get_builds(tenant, zuul_url):
    url = zuul_url + "/tenant/%s/status" % tenant
    print("Checking tenant", url)
    buildsets = set()
    for build in run(
        "curl -s %s | "
        "jq --arg tenant %s -r -f get_build_info.jq" % (url, tenant)
    ).split("\n"):
        buildset = build.split("#")[0]
        if buildset not in buildsets:
            buildsets.add(buildset)
            yield build


if __name__ == "__main__":
    try:
        import os
        token = os.environ["ZUUL_AUTH_TOKEN"]
    except Exception:
        print("ZUUL_AUTH_TOKEN env is missing, run create-token first")
        exit(1)

    parser = argparse.ArgumentParser(
        description="""
Evacuate an executor.

The provided executor will be gracefully stopped, jobs running on it dequeued
then reenqueued on other available executors."""
    )
    parser.add_argument("--executor", required=True)
    parser.add_argument(
        "--zuul-api-url", default="https://softwarefactory-project.io/zuul/api"
    )
    parser.add_argument("--scheduler", default="zs.softwarefactory-project.io")
    parser.add_argument(
        "--dry", action="store_true", help="do not run the commands")
    args = parser.parse_args()

    f = args.dry and print or partial(run, show=True)
    f("ssh %s sudo zuul-executor graceful" % args.executor)

    cmds = [
        ("zuul --auth-token '%s' %s" % (token, cmd))
        for tenant in get_tenants(args.zuul_api_url)
        for cmd in filter(
            lambda cmd: cmd.endswith(args.executor),
            get_builds(tenant, args.zuul_api_url),
        )
    ]

    if args.dry:
        print("\n".join(cmds))
        if not cmds:
            exit(0)

    with open("dequeue-script.sh", "w") as ds:
        ds.write("\n".join(cmds) + "\n")

    f(
        "scp dequeue-script.sh %s: "
        "&& ssh %s sudo bash /home/centos/dequeue-script.sh"
        % (args.scheduler, args.scheduler)
    )

    f("echo ssh %s halt" % args.executor)
