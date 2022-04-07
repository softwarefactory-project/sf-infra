#!/bin/env python3
# Copyright (C) 2022 Red Hat
# SPDX-License-Identifier: Apache-2.0

import time
import sfinfra_lib
import configparser
import jwt


def create_token(zuul_url):
    """Create a token for every tenants"""
    config = configparser.ConfigParser()
    config.read("/etc/zuul/zuul.conf")

    def config_get(name):
        return config.get("auth zuul_operator", name)

    now = time.time()
    tenants = sfinfra_lib.get_tenants(zuul_url)
    token = {'iat': now,
             'exp': now + 3600,
             'iss': config_get("issuer_id"),
             'aud': config_get("client_id"),
             'sub': 'sf-operator',
             'zuul': {'admin': tenants},
             }
    key = config_get("secret")
    auth_token = jwt.encode(
        token,
        key=key,
        algorithm="HS256")
    return auth_token


if __name__ == "__main__":
    # TODO: use argparse
    import sys
    try:
        zuul_url = sys.argv[1]
    except Exception:
        print("usage: zuul-url")
        exit(1)
    print("export ZUUL_AUTH_TOKEN='%s'" % create_token(zuul_url))
