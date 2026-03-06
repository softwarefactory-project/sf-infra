#!/bin/env python3
# Copyright © 2026 Red Hat
# SPDX-License-Identifier: Apache-2.0

from pathlib import Path
import subprocess


def usage():
    import sys

    try:
        return (sys.argv[1], Path(sys.argv[2]))
    except IndexError:
        print("usage: manage-executors.py [start|stop] /etc/sf/$ENV")
        exit(1)


def cmd(args):
    if subprocess.Popen(args).wait() != 0:
        raise RuntimeError("failed to run: " + " ".join(args))


def cmdOut(args):
    proc = subprocess.Popen(args, stdout=subprocess.PIPE)
    out = proc.communicate()
    if proc.returncode != 0:
        raise RuntimeError("failed to run: " + " ".join(args))
    return out[0].decode('utf-8')


def stop(executor):
    import time
    print(f"{executor}: pausing")
    kubectl = ["env", f"KUBECONFIG={executor}/kubeconfig",
               "kubectl", "exec", "zuul-executor-0", "--"]
    cmd(kubectl + ["zuul-executor", "pause"])
    print(f"{executor}: waiting")
    for retry in range(3600):
        builds = cmdOut(kubectl + ["ls", "/var/lib/zuul/builds"])
        print(f"{executor}: running builds: {builds}")
        if not builds:
            break
        time.sleep(1)
    print(f"{executor}: terminate")
    cmd(["env", f"KUBECONFIG={executor}/kubeconfig", "kubectl",
         "delete", "sts", "zuul-executor"])


def start(env, executor):
    print(f"{executor}: starting...")
    cmd(["go", "run", "./main.go", "deploy",
         str(env / "sf.yaml"), "--remote", str(executor / "sf.yaml")])


def main():
    (cmd, env) = usage()
    for executor in (env / "executors").iterdir():
        if cmd == "stop":
            stop(executor)
        elif cmd == "start":
            start(env, executor)
        else:
            print(f"{executor}: unknown command {cmd}")


if __name__ == "__main__":
    main()
