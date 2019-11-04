#!/bin/bash

BUP_DIR="${1}"
DIRECTORY="${2}"
USE_DATE_SUBDIR="${3}"

if [ $USE_DATE_SUBDIR -eq 1 ]; then
    CURDATE=$(date +%Y-%m)
    BUP_DIR=${BUP_DIR}/${CURDATE}
fi

export BUP_DIR

# Get timestamps
NOW=$(date +"%s")
FILE_DATE=$(sudo /usr/local/bin/bup ls -l ${DIRECTORY} | grep -v latest | awk '{print $6}' |sort -r | head -n 1)

[ -z $FILE_DATE ] && exit 1

DATE_SHORT=$(echo $FILE_DATE | awk -F- '{print $1$2$3}')
TIMESTAMP=$(date -d "${DATE_SHORT}" +%s)

echo $TIMESTAMP
