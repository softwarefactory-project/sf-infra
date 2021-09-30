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


# recursively merge d2 in d1
def merge(d1, d2):
    # If d2 is not a dict, then stop
    if not isinstance(d2, dict):
        return d2

    # create a new dict with d1
    new = dict()
    for key in d1:
        new[key] = d1[key]

    for key in d2:
        if key in new:
            new[key] = merge(d1[key], d2[key])
        else:
            new[key] = d2[key]

    return new


def list_reduce(cb, init):
    return functools.reduce(cb, init, [])


# The initial inventory
inventory = json.loads(sys.stdin.read())

# We extract the group and their hosts from the list of instances
groups = list_reduce(
    lambda acc, instance: acc + instance["groups"], inventory["instances"])
hosts_groups = dict([
    (group,
     dict(hosts=dict(list_reduce(
         lambda acc, instance: acc + [(instance["name"], {})]
         if group in instance["groups"]
         else acc,
         inventory["instances"]))))
    for group in groups])

# And the extra groups with custom children
extra_groups = dict(list(inventory.get("groups", {}).items()))

print(json.dumps(dict(all=dict(children=merge(hosts_groups, extra_groups),
                               hosts=inventory["hosts"]))))
