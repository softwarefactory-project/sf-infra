#!/usr/bin/env python

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

import argparse
from zuul.lib import encryption
import zuul.configloader
import zuul.model
import json
import zuul.lib.keystorage
import configparser
import textwrap

DESCRIPTION = """Decrypt a Zuul secret.
"""


def getProjectSecretsKeys(keyfile, path, password_bytes):
    """Return the public and private keys"""
    keys = keyfile.get("keys")
    projectkeys = keys.get(path + "/secrets")
    if projectkeys is None:
        print("Project path", path + "/secrets", "does not exist")
        exit(1)
    pk = projectkeys["keys"][0]["private_key"]
    pem_private_key = pk.encode("utf-8")
    private_key, public_key = encryption.deserialize_rsa_keypair(
        pem_private_key, password_bytes)

    return private_key, public_key


def main():
    parser = argparse.ArgumentParser(description=DESCRIPTION)
    parser.add_argument('dumppath',
                        help="Path to the zuul-admin export-keys \
                        command output")
    parser.add_argument('config',
                        help="Zuul.conf file with Keystore password")
    parser.add_argument('file',
                        help="The YAML file with secrets")
    parser.add_argument('zkpath',
                        help="Path to the project key in Zookeeper")
    parser.add_argument('--list',
                        help="list projects from exported keys",
                        action='store_true')
    args = parser.parse_args()

    keysfile = json.load(open(args.dumppath))
    if (args.list):
        for key, _ in keysfile['keys'].items():
            if "secrets" in key:
                print(key[:-8])
        exit(0)

    zuulconfig = configparser.ConfigParser()
    zuulconfig.read(args.config)
    try:
        password = zuulconfig.get("keystore", "password").encode("utf-8")
    except configparser.NoSectionError as section:
        print(section)
        exit(1)
    except configparser.NoOptionError as key:
        print(key)
        exit(1)

    priv, pub = getProjectSecretsKeys(keysfile, args.zkpath, password)

    parser = zuul.configloader.SecretParser(None)
    sc = zuul.model.SourceContext(None, 'project', None, 'master',
                                  'path', False)

    data = zuul.configloader.safe_load_yaml(open(args.file).read(), sc)
    for element in data:
        if 'secret' not in element:
            continue
        s = element['secret']
        secret = parser.fromYaml(s)
        data = secret.decrypt(priv).secret_data
        output = textwrap.dedent(
            '''
            - secret:
                name: {}
                data:
            '''.format(secret.name))

        twrap = textwrap.TextWrapper(break_long_words=False,
                                     initial_indent=' ' * 8,
                                     subsequent_indent=' ' * 10)
        for k, v in data.items():
            output += twrap.fill(k + ": " + v)
            if list(data.keys())[-1] != k:
                output += '\n'
        print(output)


if __name__ == '__main__':
    main()
