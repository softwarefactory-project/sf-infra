#!/bin/env python3
# Copyright © 2025 Red Hat
# SPDX-License-Identifier: Apache-2.0

# The goal of this script is to generate prometheus metrics for the secret ages.


def parse_secret_locations(inp):
    """Generates (secret_name, start_line, end_line) from a vars YAML file content."""
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
                if name:
                    value = f"{name}_{value}"
                if parents:
                    value = f"{'_'.join(parents)}_{value}"
                yield (value, smark.line + 1, rest[0].start_mark.line)
                tokens = rest
            # A relevant key is found
            case [
                yaml.KeyToken(),
                yaml.ScalarToken(value="name") | yaml.ScalarToken(value="user"),
                yaml.ValueToken(),
                yaml.ScalarToken(value=value),
                *rest,
            ]:
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
    for secret, start, end in parse_secret_locations(open(fp)):
        yield (secret, max(line_ages[start:end]))


def main(args):
    print("HELP sf_infra_secret_age The UNIX time of a secret last update")
    print("TYPE sf_infra_secret_age counter")
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
      token: !vault |
        $E$
        46
    """
        )
    )
    assert got == [
        ("vexx_pass", 3, 5),
        ("other_auth", 7, 9),
        ("rdo_pass", 12, 14),
        ("org_tenant_t1_token", 18, 20),
        ("org_tenant_t2_token", 22, 24),
    ]


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
