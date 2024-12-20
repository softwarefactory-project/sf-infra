#!/bin/bash

set -e
set -u
set -o pipefail

now=$(date +"%s")
thisHost=$(hostname -s)
counter=0
textCollectorFile=${textCollectorFile:-'/var/lib/node_exporter/textfile_collector/k1s_old_containers.prom'}
textCollectorDir=$(dirname "$textCollectorFile")
maxTime='86400'

if ! [ -d "$textCollectorDir" ]; then
    echo "The $textCollectorDir does not exists! Please create it first!"
    exit 1
fi

containers=$(podman ps -a --format "time={{.CreatedAt }} command={{ .Command }}" | grep -i 'sleep inf')
if [ -n "$containers" ]; then
    while IFS= read -r line; do
        # timestamp without the alphabetic time zone
        creationDate=$(echo "$line" | awk -F'=' '{print $2}' | awk '{print $1, $2, $3}')
        creationDateEpoch=$(date -d "$creationDate" +"%s")
        delta=$((now - creationDateEpoch))
        if [ $delta -gt $maxTime ]; then
            ((counter++))
        fi
    done <<< "$containers"
fi

echo "k1s_old_containers{count=\"counted_old_containers\", hostname=\"$thisHost\"} $counter" > "$textCollectorFile"
