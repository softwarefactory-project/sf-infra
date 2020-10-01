#!/bin/bash

BUP_DIR="${1}"
DIRECTORY="${2}"
USE_DATE_SUBDIR="${3}"
CONTAINER_NAME=${CONTAINER_NAME:-$4}

if [ $USE_DATE_SUBDIR -eq 1 ]; then
    CURDATE=$(date +%Y-%m)
    BUP_DIR=${BUP_DIR}/${CURDATE}
fi

export BUP_DIR

# Get timestamps
NOW=$(date +"%s")

if [ -n "${CONTAINER_NAME}" ]; then
    # NOTE: bup can not not list files in backup in mounted volume in the
    # container, but it can read when the it is in the directory.
    FILE_DATE=$(podman exec -it "${CONTAINER_NAME}" bash -c "export BUP_DIR=$BUP_DIR && bup ls -l $DIRECTORY | grep -v latest | awk '{print \$6}' |sort -r | head -n 1")
else
    FILE_DATE=$(sudo /usr/local/bin/bup ls -l ${DIRECTORY} | grep -v latest | awk '{print $6}' |sort -r | head -n 1)
fi

[ -z $FILE_DATE ] && exit 1

DATE_SHORT=$(echo $FILE_DATE | awk -F- '{print $1$2$3}')
TIMESTAMP=$(date -d "${DATE_SHORT}" +%s)

echo $TIMESTAMP
