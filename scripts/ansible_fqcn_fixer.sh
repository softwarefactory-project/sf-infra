#!/bin/bash
# Copyright (C) 2024 Red Hat
# SPDX-License-Identifier: Apache-2.0
#
#######################################################################
# Ansible FQCN Updater Script
#
# This script updates Ansible module references to their fully
# qualified class names (FQCNs) in YAML files found within the
# 'playbooks' and 'roles' directories. It replaces module names
# with their corresponding fully qualified names based on predefined
# mappings, ensuring consistent usage across the project.
#
# Usage: fix the tasks of the current directory by running:
# $PATH_TO_SFINFRA/scripts/ansible_fqcn_fixer.sh
#
# or run the following command from $PATH_TO_SFINFRA:
#
# tox -e fqcn
#######################################################################

set -e

# Array to store patterns, replacements, and the '-e' flag for the sed command
# Each element in the array represents a sed pattern (preceded by '-e'),
# followed by its replacement
patterns_replacements=()

# Function to generate regex pattern for a given module and replacement
function generate_regex_pattern() {
    local module="$1"
    local replacement="$2"
    # Exclude matches with an indentation larger than 10,
    # as they are likely false positives within literal Kubernetes
    # manifests
    echo "s/^(\s{2,10}[-]\s|\s{2,10})${module}:\$/\1${replacement}:/g"
}

# Construct patterns and replacements for the sed command
function build_patterns()
{

    # List of commonly used built-in Ansible modules in this project
    # These modules must be referenced using their fully qualified names.
    local BUILTINS=(
        add_host
        assert
        blockinfile
        command
        copy
        cron
        debug
        dnf
        fail
        file
        get_url
        git
        group
        hostname
        import_playbook
        include_role
        include_tasks
        include_vars
        lineinfile
        meta
        mount
        package
        pause
        pip
        reboot
        replace
        service
        service_facts
        set_fact
        setup
        shell
        slurp
        stat
        synchronize
        systemd
        systemd_service
        tempfile
        template
        unarchive
        uri
        user
        wait_for
        yum
        yum_repository
    )

    # Full qualified community modules
    local -A COMMUNITY=(
        [filesystem]="community.general.filesystem"
        [mysql_db]="community.mysql.mysql_db"
        [mysql_info]="community.mysql.mysql_info"
        [mysql_user]="community.mysql.mysql_user"
        [openssl_csr]="community.crypto.openssl_csr"
        [openssl_privatekey]="community.crypto.openssl_privatekey"
        [postgresql_db]="community.postgresql.postgresql_db"
        [postgresql_query]="community.postgresql.postgresql_query"
        [x509_certificate]="community.crypto.x509_certificate"
    )

    # Full qualified Ansible posix modules
    local -A ANSIBLE_POSIX=(
        [authorized_key]="ansible.posix.authorized_key"
        [firewalld]="ansible.posix.firewalld"
        [selinux]="ansible.posix.selinux"
        [sysctl]="ansible.posix.sysctl"
    )

    # Full qualified Kubernetes modules
    local -A KUBERNETES=(
        [k8s]="kubernetes.core.k8s"
    )

    local replacement=""
    local regex=""
    for module in "${BUILTINS[@]}"; do
        replacement="ansible.builtin.${module}"
        regex="$(generate_regex_pattern ${module} ${replacement})"
        patterns_replacements+=("-e")
        patterns_replacements+=("${regex}")
    done

    for key in "${!COMMUNITY[@]}"; do
        replacement="${COMMUNITY[${key}]}"
        regex="$(generate_regex_pattern ${key} ${replacement})"
        patterns_replacements+=("-e")
        patterns_replacements+=("${regex}")
    done

    for key in "${!ANSIBLE_POSIX[@]}"; do
        replacement="${ANSIBLE_POSIX[${key}]}"
        regex="$(generate_regex_pattern ${key} ${replacement})"
        patterns_replacements+=("-e")
        patterns_replacements+=("${regex}")
    done

    for key in "${!KUBERNETES[@]}"; do
        replacement="${KUBERNETES[${key}]}"
        regex="$(generate_regex_pattern ${key} ${replacement})"
        patterns_replacements+=("-e")
        patterns_replacements+=("${regex}")
    done

}


function main()
{
    build_patterns

    # Find YAML files in 'playbooks' and 'roles' directories and
    # iterate over each file to apply the sed patterns and replacements
    find playbooks roles -name "*.yml" -o -name "*.yaml" -print0 |
        while IFS= read -r -d '' file; do
            sed -E -i "${patterns_replacements[@]}" "${file}"
        done
}
