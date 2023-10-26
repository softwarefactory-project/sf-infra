#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
    echo "Error: Please provide a file as a parameter."
    exit 1
fi

if [ ! -f "$1" ]; then
    echo "Error: '$1' is not a valid file."
    exit 1
fi

if [ ! -d "vendor" ]; then
    echo "Error: Vendor folder not found. Please run 'jb install' to install dependencies."
    exit 1
fi

# Execute the command using jsonnet
jsonnet -J vendor "$1"
