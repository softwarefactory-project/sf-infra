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

"""This script re-order the yaml keys to be more idiomatic
"""

from typing import List, Tuple
import sys


def blocks(content: str) -> Tuple[str, List[List[str]]]:
    def remove_tick(line: str) -> str:
        return "  " + line[2:] if line.startswith("- ") else line

    block: List[str] = []
    result: List[List[str]] = []
    header = content[:content.index("\n-")]
    lines = content[content.index("\n-") + 1:].split("\n")
    for line in map(
            str.rstrip, filter(lambda line: line != "", lines + ["-"])):
        if line and line[0] == "-" and block:
            result.append(list(map(remove_tick, block)))
            block = [line]
        else:
            block.append(line)
    return header, result


def reorder(block: List[str]) -> List[str]:
    ordered_pos = [
        pos
        for key in ("name", "when", "become", "loop", "register")
        for (pos, elem) in enumerate(block)
        if elem.startswith("  %s:" % key)
    ]
    rest_pos = [
        pos for pos in range(len(block))
        if pos not in ordered_pos
    ]
    return list(map(lambda pos: block[pos], ordered_pos + rest_pos))


def unblocks(blocks: List[List[str]]) -> str:
    def add_tick(block: str) -> str:
        return "-" + block[1:]

    return "\n\n".join([
        add_tick("\n".join(block)) for block in blocks
    ])


if __name__ == "__main__":
    if sys.argv[1:]:
        inp = open(sys.argv[1]).read()
    else:
        inp = sys.stdin.read()
    header, blocks = blocks(inp)
    result = header + "\n" + unblocks(
        list(map(reorder, blocks))
    )
    if sys.argv[1:]:
        open(sys.argv[1], "w").write(result + "\n")
    else:
        print(result)
