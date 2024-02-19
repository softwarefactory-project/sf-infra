#!/bin/bash

# Source the script with the correct relative path
source "$(dirname "$0")/../ansible_fqcn_fixer.sh"

# Run a test case
test_regex() {
    local regex="$(generate_regex_pattern key replacement)"
    local input="$1"
    local expected_output="$2"
    local description="$3"

    local actual_output=$(echo "${input}" | sed -E "${regex}")

    if [[ "${actual_output}" == "${expected_output}" ]]; then
        echo "PASS: Input '${input}' matches expected output '${expected_output}'" "${description}"
    else
        echo "FAIL: Input '${input}' does not match expected output '${expected_output}'" "${description}"
        echo "Actual output: '${actual_output}'"
        exit 1
    fi
}

# Test cases for patterns_replacements regex
run_tests() {
    local description="Replacement: key is replaced"
    # Cases with replacement
    test_regex "  - key:" \
               "  - replacement:" \
               "${description}"
    test_regex "    - key:" \
               "    - replacement:" \
               "${description}"
    test_regex "          - key:" \
               "          - replacement:" \
               "${description}"

    # Cases with no replacement
    local test_case=" - key:"
    test_regex "${test_case}" "${test_case}" \
               "No replacement: Indentation < 2 spaces"
    test_case="            - key:"
    test_regex "${test_case}" "${test_case}" \
               "No replacement: Indentation > 10 spaces"
    test_case="    -   key:"
    test_regex "${test_case}" "${test_case}" \
               "No replacement: Wrong number of spaces between '-' and key"
    test_case="    - key:value"
    test_regex "${test_case}" "${test_case}" \
               "No replacement: there must be an endline after the key"
}

run_tests
