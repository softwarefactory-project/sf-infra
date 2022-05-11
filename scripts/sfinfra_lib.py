#!/bin/env python3
# Copyright (C) 2022 Red Hat
# SPDX-License-Identifier: Apache-2.0

import subprocess


def run(cmd, show=False):
    o = subprocess.check_output(cmd, shell=True)
    if show:
        print(o)
    return o.decode("utf-8").strip()


def get_tenants(zuul_url):
    url = zuul_url + "/tenants"
    return run("curl -s %s | jq -r '.[] | .name'" % url).split("\n")
