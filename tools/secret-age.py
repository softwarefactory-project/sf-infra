#!/bin/env python3
# Copyright © 2025 Red Hat
# SPDX-License-Identifier: Apache-2.0

# The goal of this script is to generate prometheus metrics for the secret ages.


def collect_previous_comments(buf, pos):
    """Extract the comment in the buffer before the pos"""

    def drop_previous_line(pos):
        while pos >= 0 and buf[pos] == " ":
            pos -= 1
        if pos < 0 or buf[pos] != "\n":
            return -1
        return pos

    pos = drop_previous_line(pos) - 1
    end_pos = pos
    while pos >= 0 and buf[pos] not in ["#", "\n"]:
        pos -= 1

    is_comment = (
        buf[pos] == "#"
        if pos == 0
        else pos > 0 and buf[pos] == "#" and buf[pos - 1] in [" ", "\n"]
    )
    if is_comment and (pos == 0 or drop_previous_line(pos - 1) != -1):
        comment = buf[pos + 2 : end_pos + 1].strip()
        if prev := collect_previous_comments(buf, pos - 1):
            comment = f"{prev}\n{comment}"
        return comment


def parse_time(comment):
    import time

    for line in comment.split("\n") if comment else []:
        match line.split(":"):
            case ["refreshed" | "refreshed_date", s]:
                return int(time.mktime(time.strptime(s.strip(), "%Y-%m-%d")))
    return None


def parse_secret_locations(inp):
    """Generates (secret_name, start_line, end_line, optional age) from a YAML file."""
    import yaml

    tokens = list(yaml.scan(inp))
    # The names of the parent keys
    parents = []
    # The descriptive name of the current item
    name = None
    # Keep track of nested structure, starts at -1 because the document begin with one
    nested_count = -1
    while tokens:
        match tokens:
            # A new secret begin
            case [
                yaml.KeyToken(),
                yaml.ScalarToken(value=value, start_mark=smark),
                yaml.ValueToken(),
                yaml.TagToken(value=("!", "vault")),
                yaml.ScalarToken(),
                *rest,
            ]:
                refreshed = parse_time(
                    collect_previous_comments(smark.buffer, smark.pointer - 1)
                )
                if name:
                    value = f"{name}_{value}"
                parents_name = list(
                    filter(lambda n: n not in ["vars", "tasks"], parents)
                )
                if parents_name:
                    value = f"{'_'.join(parents_name)}_{value}"
                yield (value, smark.line + 1, rest[0].start_mark.line, refreshed)
                tokens = rest
            # A refresh date that is hard-coded without a associated !vault
            case [
                yaml.KeyToken(),
                yaml.ScalarToken(value=value, start_mark=smark),
                yaml.ValueToken(),
                yaml.ScalarToken(),
                *rest,
            ] if value.endswith("_refreshed_date"):
                value = value[: -len("_refreshed_date")]
                yield (value, smark.line + 1, smark.line + 1)
                tokens = rest

            # A relevant key is found
            case [
                yaml.KeyToken(),
                yaml.ScalarToken(value="name") | yaml.ScalarToken(value="user"),
                yaml.ValueToken(),
                yaml.ScalarToken(value=value),
                *rest,
            ] if (
                " " not in value
            ):
                name = value
                tokens = rest
            # A new dictionary begin, collect it's parent key
            case [
                yaml.KeyToken(),
                yaml.ScalarToken(value=value),
                yaml.ValueToken(),
                yaml.BlockMappingStartToken() | yaml.BlockSequenceStartToken(),
                *rest,
            ]:
                parents.append(value)
                tokens = rest
            # A new list item begin
            case [yaml.BlockMappingStartToken(), *rest]:
                nested_count += 1
                tokens = rest
            # A dictionary end, pop the parent key
            case [yaml.BlockEndToken(), *rest]:
                if nested_count > 0:
                    nested_count -= 1
                elif parents:
                    parents.pop()
                name = None
                tokens = rest
            case _:
                tokens = tokens[1:]


def parse_git_blame(out):
    """Generates the line ages from a git blame output."""
    for line in out.split("\n")[:-1]:
        match line.split():
            case [_, _, _, age, *_]:
                yield int(age)
            case args:
                raise RuntimeError(f"Unknown git blame output: {args}")


def read_git_blame(fp):
    """Returns the git blame output."""
    import subprocess

    return subprocess.check_output(["git", "blame", "--show-name", "-te", fp])


def git_blame(fp):
    return list(parse_git_blame(read_git_blame(fp).decode("utf-8")))


def process(fp):
    """Generate (secret_name, secret_age) for a given vars YAML file."""
    line_ages = git_blame(fp)
    for secret, start, end, age in parse_secret_locations(open(fp).read()):
        yield (secret, age or max(line_ages[start:end]))


def main(args):
    print("# HELP sf_infra_secret_age_total The UNIX time of a secret last update")
    print("# TYPE sf_infra_secret_age_total counter")
    for fp in args:
        for secret, age in process(fp):
            print("""sf_infra_secret_age_total{name="%s"} %d""" % (secret, age))


def test():
    got = list(
        parse_secret_locations(
            """
vexx:
    pass: !vault |
      $A$
      42
    user: zuul
other_auth: !vault |
  $B$
  43
rdo:
    url: rdoproject.org
    pass: !vault |
      $C$
      44
org:
  tenant:
    - name: t1
      token: !vault |
        $D$
        45
    - name: t2
      # refreshed: 2025-05-21
      token: !vault |
        $E$
        46
rhn_refreshed_date: 2025-12-01
tasks:
  - name: "a task"
    vars:
      task_secret: !vault |
        $F$
        47
    """
        )
    )
    assert got == [
        ("vexx_pass", 3, 5, None),
        ("other_auth", 7, 9, None),
        ("rdo_pass", 12, 14, None),
        ("org_tenant_t1_token", 18, 20, None),
        ("org_tenant_t2_token", 23, 25, 1747785600),
        ("rhn", 26, 26),
        ("task_secret", 30, 32, None),
    ], got


if __name__ == "__main__":
    import os
    import sys

    args = sys.argv[1:]
    if not args or args == ["--help"]:
        print("usage: secret-age.py PATH...")
        exit(1)
    if args == ["test"]:
        test()
    elif not all(map(os.path.exists, args)):
        print(f"Error: unknown file: {', '.join(args)}")
        exit(1)
    else:
        main(args)
