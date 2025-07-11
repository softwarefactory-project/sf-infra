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
import json
import configparser

DESCRIPTION = """Migrate Zookeeper project secret keys from a Zuul instance
                 to another Zuul instance, with two different Keystore values.
"""


def getKeyStore(filename: str) -> str:

    zuulconfig = configparser.ConfigParser()
    zuulconfig.read(filename)

    try:
        keyStorePassword = zuulconfig.get(
            "keystore", "password").encode("utf-8")
    except configparser.NoSectionError as section:
        print(section)
        exit(1)
    except configparser.NoOptionError as key:
        print(key)
        exit(1)

    return keyStorePassword


def serialize_zk_dump(keyfile, src_pass_bytes, dest_pass_bytes) -> dict:
    """Return the public and private keys"""
    keys = keyfile.get("keys")

    for key in keys:
        projectkey = keys.get(key)
        if 'keys' not in projectkey:
            continue
        encryptedKeysList = projectkey.get("keys")
        for encryptedKey in encryptedKeysList:
            pem_private_key = encryptedKey.get("private_key").encode("utf-8")
            private_key, public_key = encryption.deserialize_rsa_keypair(
                pem_private_key, src_pass_bytes)
            encrypted_private_key = encryption.serialize_rsa_private_key(
                private_key, dest_pass_bytes)
            encryptedKey["private_key"] = encrypted_private_key.decode("utf-8")

    return keyfile


def getProjectSecretsKeys(keyfile, path, password_bytes):
    """Return the public and private keys"""
    keys = keyfile.get("keys")
    projectkeys = keys.get(path + "/secrets")
    if projectkeys is None:
        print(f"Project path {path} does not exist")
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
                        command output.")
    parser.add_argument('origin_config',
                        help="Zuul.conf file with Keystore password from \
                        the origin Zuul Instance.")
    parser.add_argument('dest_config',
                        help="Zuul.conf file with Keystore password from \
                       the destination Zuul Instance.")
    args = parser.parse_args()

    keysfile = json.load(open(args.dumppath))

    originKeyStorePass = getKeyStore(args.origin_config)
    destKeyStorePass = getKeyStore(args.dest_config)

    serializedKeysfile = serialize_zk_dump(
        keysfile, originKeyStorePass, destKeyStorePass)
    print(json.dumps(serializedKeysfile))


if __name__ == '__main__':
    main()
