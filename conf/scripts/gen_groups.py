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

"""This script creates the syntaxic sugar related to the group sum type.
"""

import sys

# Groups is the list of group as str
groups = list(map(str.strip, open(sys.argv[1]).read().strip(
    )[1:-1].split('|')))

print("\n".join([
    "{- This file is generated by python -}",
    "let Group = ./Group.dhall in { ",
    # First we render a show method to transform a group to Text
    "show = \\(group : Group) -> merge { %s } group, " % (
        " , ".join([group + " = \"" + group + "\"" for group in groups])),
    # Then we render a list of all the groups with a test function
    "groups = [ %s ], " % " , ".join([
        ("{ value = Group.%s , "
         "test = \\(group : Group) -> merge { %s } group }") % (
            group, " , ".join([other_group + " = " + str(group == other_group)
                               for other_group in groups]))
        for group in groups]
    ),
    "Type = { value : Group, test : forall (group : Group) -> Bool }}"]))
