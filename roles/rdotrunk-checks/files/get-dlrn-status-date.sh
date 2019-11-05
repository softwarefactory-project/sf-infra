#!/bin/bash

WORKER="${1}"
TARGET="${2}"

TIMESTAMP=$(stat -c %Y /home/${WORKER}/data/repos/${TARGET}/delorean.repo)
[ -z $TIMESTAMP ] && echo 0 && exit 1
echo $TIMESTAMP
