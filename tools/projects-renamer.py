#!/bin/env python3
# Copyright © 2025 Red Hat
# SPDX-License-Identifier: Apache-2.0

"""
To rename a project:

1. zuul-admin export-keys ./zk.dmp
2. Create a JSON file with an array at root, listing all the projects to rename, like:
```JSON
[
    # 1 Project
        {
        # Project at source
                "source": {
                        "connection_name" : "gerrit",
                        "name" : "project_name"
                },
        # Project at target
                "target": {
                        "connection_name" : "gitlab",
                        "name" : "project_name"
                },
        },
        ...
]
```
3. ./projects-renamer.py ./zk.dmp <path to JSON file>
4. zuul-admin import-keys ./new-<zookeeper dump>
"""

import argparse
import json
import zuul.lib.keystorage as zlk
import os


def get_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("zookeeper_dump", help="Zookeeper Dump File")
    parser.add_argument("json_file", help="JSON File with a list of projects to rename")

    args = parser.parse_args()
    return args


def get_path(keystorage, connection, proj):
    return (
        keystorage.getProjectSecretsKeysPath(keystorage, connection, proj).rsplit(
            "/", 1
        )[0] + "/"
    )


def changeProjectName(proj_list, project):
    project_split = os.path.split(project[0])
    project_name = project_split[0] + "/"

    if project_name in proj_list:
        target = os.path.join(proj_list[project_name], project_split[1])
        print(f"Renaming {project[0]} to {target}")
        return (project[0].replace(project[0], target), project[1])

    return project


def filterProjectName(proj_list, project):
    project_split = os.path.split(project[0])[0] + "/"
    if project_split in proj_list:
        return True
    else:
        return False


def main(args):

    ks = zlk.KeyStorage

    zk_dump_path = os.path.realpath(args.zookeeper_dump)
    zk_new_dump_path = os.path.join(
        os.path.dirname(zk_dump_path), "new-" + os.path.basename(zk_dump_path)
    )

    project_list_file = os.path.realpath(args.json_file)

    proj_mig_list = {
        get_path(
            ks, proj["source"]["connection_name"], proj["source"]["name"]
        ): get_path(ks, proj["target"]["connection_name"], proj["target"]["name"])
        for proj in json.load(open(project_list_file))
    }

    items = json.load(open(zk_dump_path))["keys"].items()

    open(zk_new_dump_path, "w").write(
        json.dumps(
            dict(
                keys=dict(
                    map(
                        lambda ele: changeProjectName(proj_mig_list, ele),
                        filter(
                            lambda ele: filterProjectName(proj_mig_list, ele), items
                        ),
                    )
                )
            ),
        )
    )
    print(f"\nNew Zookeeper dump: {zk_new_dump_path}")


if __name__ == "__main__":
    main(get_args())
