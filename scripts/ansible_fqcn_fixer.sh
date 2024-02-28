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
    echo "s/^(-\s{1,2}|\s{2,10}[-]\s|\s{2,10})${module}:\$/\1${replacement}:/g"
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
        known_hosts
        iptables
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

    # Full qualified openstack cloud modules
    local OPENSTACK_CLOUD=(
        os_address_scope
        os_auth
        os_baremetal_deploy_template
        os_baremetal_inspect
        os_baremetal_node
        os_baremetal_node_action
        os_baremetal_node_info
        os_baremetal_port
        os_baremetal_port_info
        os_catalog_service
        os_catalog_service_info
        os_coe_cluster
        os_coe_cluster_template
        os_compute_flavor
        os_compute_flavor_access
        os_compute_flavor_info
        os_compute_service_info
        os_config
        os_dns_zone
        os_dns_zone_info
        os_endpoint
        os_federation_idp
        os_federation_idp_info
        os_federation_mapping
        os_federation_mapping_info
        os_floating_ip
        os_floating_ip_info
        os_group_assignment
        os_host_aggregate
        os_identity_domain
        os_identity_domain_info
        os_identity_group
        os_identity_group_info
        os_identity_role
        os_identity_role_info
        os_identity_user
        os_identity_user_info
        os_image
        os_image_info
        os_keypair
        os_keypair_info
        os_keystone_federation_protocol
        os_keystone_federation_protocol_info
        os_lb_health_monitor
        os_lb_listener
        os_lb_member
        os_lb_pool
        os_loadbalancer
        os_network
        os_networks_info
        os_neutron_rbac_policies_info
        os_neutron_rbac_policy
        os_object
        os_object_container
        os_port
        os_port_info
        os_project
        os_project_info
        os_quota
        os_recordset
        os_resource
        os_resources
        os_role_assignment
        os_router
        os_routers_info
        os_security_group
        os_security_group_info
        os_security_group_rule
        os_security_group_rule_info
        os_server
        os_server_action
        os_server_group
        os_server_info
        os_server_metadata
        os_server_volume
        os_stack
        os_stack_info
        os_subnet
        os_subnet_pool
        os_subnets_info
        os_volume
        os_volume_backup
        os_volume_backup_info
        os_volume_info
        os_volume_snapshot
        os_volume_snapshot_info
        os_volume_type
        os_volume_type_access
        os_volume_type_encryption
        os_volume_type_info
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
        [openshift_raw]="kubernetes.core.k8s"
    )

    local replacement=""
    local regex=""
    for module in "${BUILTINS[@]}"; do
        replacement="ansible.builtin.${module}"
        regex="$(generate_regex_pattern ${module} ${replacement})"
        patterns_replacements+=("-e")
        patterns_replacements+=("${regex}")
    done

    for module in "${OPENSTACK_CLOUD[@]}"; do
        replacement="openstack.cloud.${module#os_}"
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
