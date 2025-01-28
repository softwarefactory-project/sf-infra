# Copyright © 2025 pcinfra
# SPDX-License-Identifier: Apache-2.0

"""
Use this script to rename a project:

  zuul export-keys ./zk.dmp
  ./project-renamer.py old-conn old-proj new-conn new-proj
  zuul import-keys ./new.dmp
"""

import json
import zuul.lib.keystorage
import sys

ks = zuul.lib.keystorage.KeyStorage(None, 'secret', None)


def get_path(connection, proj):
    return ks.getProjectSecretsKeysPath(
        connection, proj).rsplit('/', 1)[0] + '/'


old = get_path(sys.argv[1], sys.argv[2])
new = get_path(sys.argv[3], sys.argv[4])

print("Renaming from ", old, new)
open('new.dmp', 'w').write(json.dumps(
    dict(keys=dict(
        # Rename old with new
        map(lambda kv: (kv[0].replace(old, new), kv[1]),
            # Keep only entry matching old
            filter(lambda kv: kv[0].startswith(old),
                   # Load the zookeeper dump
                   json.load(open('zk.dmp'))["keys"].items()))))))
