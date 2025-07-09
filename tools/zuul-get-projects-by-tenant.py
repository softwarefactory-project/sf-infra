#!/bin/env python3
# Copyright © 2024 pcinfra
# SPDX-License-Identifier: Apache-2.0

import json
import requests
import os
import argparse


def get_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("zuul_root_url", help="Zuul Root Url")
    parser.add_argument("tenant", help="Tenant name")

    args = parser.parse_args()
    return args


def main(args):

    tenant_name = args.tenant
    api_call_address = f"{args.zuul_root_url}/api/tenant/\
{tenant_name}/projects"
    projects = requests.get(api_call_address)
    converted_projects = [
        {
            "source": {
                "connection_name": proj["connection_name"],
                "name": proj["name"],
            },
            "target": {
                "connection_name": proj["connection_name"],
                "name": proj["name"],
            },
        }
        for proj in json.loads(projects.text)
    ]

    written_file = os.path.join(
        os.path.realpath(os.path.curdir), f"{tenant_name}-tenant-projects.json"
    )

    open(written_file, "w").write(json.dumps(converted_projects))

    print(f"{tenant_name} projects written at: {written_file}")


if __name__ == "__main__":
    main(get_args())
