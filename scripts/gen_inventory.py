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

"""This script creates ansible inventory by merging the group keys"""
import functools
import json
import sys


inventory = json.loads(sys.stdin.read())
print(json.dumps(dict(all=dict(
    children=dict([
        # We extract the group and their hosts from the list of instances
        (group, dict(
            hosts=dict(
                functools.reduce(
                    lambda acc, instance: acc + [(instance["name"], {})]
                    if group in instance["groups"] else acc,
                    inventory["instances"],
                    [],
                )
            )
        ))
        for group in functools.reduce(
                lambda acc, instance: acc + instance["groups"],
                inventory["instances"],
                [])
    ]   # The extra groups and hosts can be used as-it
        + list(inventory.get("groups", {}).items())
    ), hosts=inventory["hosts"]))))
