#!/bin/bash

URL="${1}"

TIMESTAMP=$(curl -k $URL 2>/dev/null | jq '.[0].datapoints[0][0]')
[ -z $TIMESTAMP ] && echo 0 && exit 1
echo $TIMESTAMP
