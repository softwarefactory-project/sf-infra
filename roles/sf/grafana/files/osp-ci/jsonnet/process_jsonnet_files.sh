#!/usr/bin/env bash

SCRIPT_DIR="$(dirname $(readlink -f ${BASH_SOURCE[0]}))"

usage() {
    cat <<EOF
Usage: $0 <vendor_directory>

Generate JSON Grafana models based on existing ci-dashboard* jsonnet files in the parent folder.

Arguments:
  <vendor_directory>   Path to the vendor directory containing grafonnet library (https://github.com/grafana/grafonnet).

Example:
  $0 /path/to/vendor_folder
EOF
}

if [ "$#" -ne 1 ]; then
    usage
    exit 1
fi

vendor_directory="$1"

if [ ! -d "$vendor_directory" ]; then
    echo "Error: Vendor directory '$vendor_directory' does not exist."
    exit 1
fi

# Generate the JSON Grafana models per ci-dashboard jsonnet file in the parent folder
for file in "$SCRIPT_DIR"/ci-dashboard*.jsonnet; do
    filename=$(basename "$file")
    output_file="$SCRIPT_DIR/../${filename%.jsonnet}.json"

    if [ -f "$file" ]; then
        if jsonnet_output=$(jsonnet -J "$vendor_directory" "$file" 2>&1); then
            echo "$jsonnet_output" > "$output_file.tmp"
            mv "$output_file.tmp" "$output_file"
            echo "Generated: $output_file"
        else
            echo "Error: Failed to generate '$output_file'. Details:"
            echo "$jsonnet_output"
            rm "$output_file.tmp" 2>/dev/null
        fi
    fi
done

echo "Script execution completed."