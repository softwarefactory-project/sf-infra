#!/bin/env python3
# Copyright © 2025 Red Hat
# SPDX-License-Identifier: Apache-2.0

# The goal of this script is to generate prometheus metrics for the secret ages.
# The secret metric will be set to 0 when no time marker is found.

import yaml
import sys
import pathlib


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
                yaml.ScalarToken(value=date),
                *rest,
            ] if value.endswith("_refreshed_date"):
                value = value[: -len("_refreshed_date")]
                yield (
                    value,
                    smark.line,
                    smark.line + 1,
                    parse_time(f"refreshed: {date}"),
                )
                tokens = rest

            # A relevant key is found
            case [
                yaml.KeyToken(),
                yaml.ScalarToken(value="name") | yaml.ScalarToken(value="user"),
                yaml.ValueToken(),
                yaml.ScalarToken(value=value),
                *rest,
            ] if " " not in value:
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


def process(fp):
    """Generate (secret_name, secret_age) for a given vars YAML file."""
    for secret, start, end, age in parse_secret_locations(open(fp).read()):
        yield (secret, age)


def read_rules_secrets(start):
    secret_file = start.parent / "monitoring/rules-secrets.yaml"
    if secret_file.exists():
        return secret_file.read_text()
    elif start.parent == "/":
        raise RuntimeError("Couldn't find rules-secrets.yaml")
    else:
        return read_rules_secrets(start.parent)


def decode_rules_secret(rules):
    import re

    m_re = re.compile(r".*sf_infra_secret_age_total{name=~'([^']+)'}.*")
    matchers = []
    for group in rules["groups"]:
        for rule in group["rules"]:
            if m := m_re.match(rule["expr"]):
                matchers.append(re.compile("^(" + m[1] + ")$"))
    return matchers


def print_errors(msg, errors):
    if errors:
        print(f"{msg}\n{'=' * len(msg)}", file=sys.stderr)
        for error in errors:
            print(error, file=sys.stderr)


def main(args):
    print("# HELP sf_infra_secret_age_total The UNIX time of a secret last update")
    print("# TYPE sf_infra_secret_age_total gauge")
    matchers = decode_rules_secret(
        yaml.safe_load(read_rules_secrets(pathlib.Path(args[0])))
    )
    match_errors = []
    expiry_errors = []
    for fp in args:
        rel_fp = fp.split("/sf-infra/", 1)[1] if "/sf-infra/" in fp else fp
        for secret, age in process(fp):
            if not any(map(lambda matcher: matcher.match(secret), matchers)):
                match_errors.append(f"{fp}: {secret}")
            if not age:
                expiry_errors.append(f"{fp}: {secret}")
            else:
                print(
                    """sf_infra_secret_age_total{name="%s", file="%s"} %d"""
                    % (secret, rel_fp, age)
                )
    if match_errors or expiry_errors:
        print_errors(
            "Vault variables without secret expiry or description found:", match_errors
        )
        print_errors("Secret without a refresh date found:", expiry_errors)
        exit(1)


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
    - name: t3
      # refreshed_date: 2025-05-22
      token: !vault |
        $E$
        48
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
        ("org_tenant_t3_token", 28, 30, 1747872000),
        ("rhn", 30, 31, 1764547200),
        ("task_secret", 35, 37, None),
    ], got


if __name__ == "__main__":
    import os

    # Ensure timestamps are parsed as UTC
    os.environ["TZ"] = "UTC"

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
