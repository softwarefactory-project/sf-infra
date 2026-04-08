#!/bin/bash
#
# Run CentOS Infra automation from bridge.softwarefactory-project.io (same split as downstream run-infra-ng.sh).
#
# Creates ~/ansible.cfg -> <repo>/ansible/ansible.cfg so ANSIBLE_CONFIG matches the bridge layout.
# Vault password: ansible.cfg uses vault_password_file = ~/.ansible_vault (e.g. /home/cloud-user/.ansible_vault).
#
# Example:
#   cd ~/src/softwarefactory-project.io/software-factory/sf-infra && ./run-centosinfra.sh k8s
#
# Go toolchain on this host (pin in playbooks/host_vars/bridge.softwarefactory-project.io.yaml):
#   ./run-centosinfra.sh go
#
# site-centosinfra-vms uses role sf/setup-microshift, which include_roles the external
# openstack-k8s-operators/ansible-microshift-role (see roles/sf/setup-microshift/defaults).
# We clone it like run-infra-ng.sh and pass ansible_microshift_role_dir (not needed for action k8s).

set -ex

BRIDGE_FQDN="bridge.softwarefactory-project.io"
REPO_ROOT=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
PLAYBOOK_LOG_DIR="${PLAYBOOK_LOG_DIR:-/tmp}"
LOG_ROOT="${LOG_ROOT:-/var/log/run-centosinfra}"
CACHE_GIT="${HOME}/.cache/run-centosinfra/git"
MICROROLE="${CACHE_GIT}/roles/ansible-microshift-role"
# Match zuul.d sf-infra-configure-centOS-Infra required-projects override-checkout; override to use master like run-infra-ng.sh.
ANSIBLE_MICROSHIFT_ROLE_VERSION="${ANSIBLE_MICROSHIFT_ROLE_VERSION:-4.16.0-4}"
EXTRA_VARS=""

usage() {
    cat <<EOF
Usage: $0 action

action:
  vms   - provision centos-infra-zuul-executors (RHEL, MicroShift, node-exporter)
  k8s   - prepare /etc/sf/prod-centos and apply manifests + sf-operator (bridge + image-builder)
  go    - install or upgrade pinned Go on this bridge (playbooks/site-centosinfra-bridge-go.yaml)
  all   - vms then k8s (equivalent to playbooks/site_centOSInfra.yaml)

Environment:
  PLAYBOOK_LOG_DIR               - directory for the initial ansible log under /tmp (default: /tmp)
  LOG_ROOT                       - where the log is moved after the run (default: /var/log/run-centosinfra)
  ANSIBLE_MICROSHIFT_ROLE_VERSION - git ref for ansible-microshift-role (default: 4.16.0-4, same as Zuul pin)
  PULL_SECRET_PATH               - YAML with openshift_pull_secret for MicroShift (default: ~/.ansible_crc_vars.yaml)
  RUN_CENTOSINFRA_SKIP_PULL_SECRET_CHECK - set to 1 to skip the pull-secret file check
EOF
    exit 1
}

if [ "${1:-}" = "" ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    usage
fi

# Check for dependency tooling
type -f ansible ansible-playbook ansible-galaxy || {
    echo "Please install ansible-core (sudo dnf -y install ansible-core)" >&2
    exit 1
}

# Symlink repo ansible.cfg to ~/ansible.cfg (see comment above)
CFG_LINK="${HOME}/ansible.cfg"
if [ -e "$CFG_LINK" ] && [ ! -L "$CFG_LINK" ]; then
    echo "Refusing to replace non-symlink ${CFG_LINK}; remove it or move it aside." >&2
    exit 1
fi
ln -sf "${REPO_ROOT}/ansible/ansible.cfg" "$CFG_LINK"
export ANSIBLE_CONFIG="$CFG_LINK"

ansible-galaxy collection install -r "${REPO_ROOT}/requirements.yml"

# Check for the last version of u/s sf-infra
git -C "$REPO_ROOT" fetch origin
if git -C "$REPO_ROOT" rev-parse origin/main >/dev/null 2>&1; then
    if [ "$(git -C "$REPO_ROOT" rev-parse HEAD)" != "$(git -C "$REPO_ROOT" rev-parse origin/main)" ]; then
        echo "You are not using the latest origin/main version, are you sure you want to continue?"
        read -r _ || true
    fi
fi

SHORTHASH=$(git -C "$REPO_ROOT" log --pretty=%h -1)

# Check for the bridge hostname
if [ "$(hostname)" != "$BRIDGE_FQDN" ]; then
    echo "This script must be run on ${BRIDGE_FQDN} (got: $(hostname -f 2>/dev/null || hostname))." >&2
    exit 1
fi

case "$1" in
    vms) PLAYBOOK="playbooks/site-centosinfra-vms.yaml" ;;
    k8s) PLAYBOOK="playbooks/site-centosinfra-k8s.yaml" ;;
    go) PLAYBOOK="playbooks/site-centosinfra-bridge-go.yaml" ;;
    all) PLAYBOOK="playbooks/site_centOSInfra.yaml" ;;
    *) usage ;;
esac

# Fetch ansible-microshift-role (required by sf/setup-microshift on executor hosts; skip for k8s-only and go-only).
if [ "$1" != "k8s" ] && [ "$1" != "go" ]; then
    # Same default as roles/sf/setup-microshift (pull_secret_path); expand leading ~ for -f test.
    _pull_secret="${PULL_SECRET_PATH:-${HOME}/.ansible_crc_vars.yaml}"
    _pull_secret="${_pull_secret/#\~/${HOME}}"
    if [ "${RUN_CENTOSINFRA_SKIP_PULL_SECRET_CHECK:-0}" != "1" ]; then
        if [ ! -f "$_pull_secret" ] || [ ! -r "$_pull_secret" ]; then
            echo "Missing or unreadable OpenShift pull secret file: ${_pull_secret}" >&2
            echo "sf/setup-microshift needs YAML with openshift_pull_secret (see roles/next-gen/prepare-host/templates/ansible_crc_vars.yaml.j2)." >&2
            echo "Zuul configure-hosts creates ~/.ansible_crc_vars.yaml from crc_secret; or set PULL_SECRET_PATH, or RUN_CENTOSINFRA_SKIP_PULL_SECRET_CHECK=1." >&2
            exit 1
        fi
    fi
    mkdir -p "${CACHE_GIT}/roles"
    ansible -m git -a "repo=https://github.com/openstack-k8s-operators/ansible-microshift-role update=true version=${ANSIBLE_MICROSHIFT_ROLE_VERSION} force=true dest=${MICROROLE}" localhost
    EXTRA_VARS="-e ansible_microshift_role_dir=${MICROROLE} -e pull_secret_path=${_pull_secret}"
fi

TIMESTAMP=$(date -Iminutes 2>/dev/null || date)
PLAYBOOK_LOG="${TIMESTAMP}-${SHORTHASH}-$1-${USER:-unknown}.log"

sudo mkdir -p "$LOG_ROOT"
sudo mkdir -p /var/lib/node_exporter/textfile_collector

# Same pattern as run-infra-ng.sh (node_exporter textfile counters)
if [ ! -f "/var/lib/node_exporter/textfile_collector/run_centosinfra_started_${1}_${USER}.prom" ]; then
    echo "run_centosinfra_start_count{who=\"${USER}\",playbook=\"$1\"} 0" | sudo tee "/var/lib/node_exporter/textfile_collector/run_centosinfra_started_${1}_${USER}.prom"
fi

oldStartCount=$(cut -d ' ' -f2 "/var/lib/node_exporter/textfile_collector/run_centosinfra_started_${1}_${USER}.prom")
newStartCount=$((oldStartCount + 1))
echo "run_centosinfra_start_count{who=\"${USER}\",playbook=\"$1\"} ${newStartCount}" | sudo tee "/var/lib/node_exporter/textfile_collector/run_centosinfra_started_${1}_${USER}.prom"

flock --conflict-exit-code 199 -n /tmp -c "cd \"${REPO_ROOT}\" && env ANSIBLE_CONFIG=\"${ANSIBLE_CONFIG}\" ANSIBLE_LOG_PATH=\"${PLAYBOOK_LOG_DIR}/${PLAYBOOK_LOG}\" ansible-playbook -v -i ansible/hosts.yaml ${EXTRA_VARS} \"${PLAYBOOK}\""
playbookExitCode=$?

touch "${HOME}/.ansible.log"
if [ -f "${PLAYBOOK_LOG_DIR}/${PLAYBOOK_LOG}" ]; then
    cat "${PLAYBOOK_LOG_DIR}/${PLAYBOOK_LOG}" >> "${HOME}/.ansible.log"
    sudo mv "${PLAYBOOK_LOG_DIR}/${PLAYBOOK_LOG}" "${LOG_ROOT}/${PLAYBOOK_LOG}"
fi

if [ ! -f "/var/lib/node_exporter/textfile_collector/run_centosinfra_ended_${1}_${USER}_${playbookExitCode}.prom" ]; then
    echo "run_centosinfra_end_count{who=\"${USER}\",playbook=\"$1\",result=\"${playbookExitCode}\"} 0" | sudo tee "/var/lib/node_exporter/textfile_collector/run_centosinfra_ended_${1}_${USER}_${playbookExitCode}.prom"
fi

oldEndCount=$(cut -d ' ' -f2 "/var/lib/node_exporter/textfile_collector/run_centosinfra_ended_${1}_${USER}_${playbookExitCode}.prom")
newEndCount=$((oldEndCount + 1))
echo "run_centosinfra_end_count{who=\"${USER}\",playbook=\"$1\",result=\"${playbookExitCode}\"} ${newEndCount}" | sudo tee "/var/lib/node_exporter/textfile_collector/run_centosinfra_ended_${1}_${USER}_${playbookExitCode}.prom"

exit "$playbookExitCode"
