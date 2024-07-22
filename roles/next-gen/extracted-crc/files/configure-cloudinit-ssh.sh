#!/bin/bash

set -x

USER_DIR=${USER_DIR:-'/var/home/core'}
CLOUD_INIT_KEYS_URL=${CLOUD_INIT_KEYS_URL:-'http://169.254.169.254/latest/meta-data/public-keys'}
USERDATA_DIR=${USERDATA_DIR:-'/tmp/openstack-config-drive'}
USERDATA_KEY_DIR=${USERDATA_KEY_DIR:-'/tmp/ssh-pub-keys'}

# inject alternative SSH keys
mkdir -p "$USERDATA_KEY_DIR" && \
    chmod 0755 "$USERDATA_KEY_DIR" && \
    sudo chown core:core "$USERDATA_KEY_DIR"

# if there is already a zuul authorized key, change the SELinux label
if [ -f "${USER_DIR}/.ssh/authorized_keys.d/zuul" ]; then
    chcon system_u:object_r:ssh_home_t:s0 "${USER_DIR}/.ssh/authorized_keys.d/zuul"
fi

cd "$USERDATA_KEY_DIR"

# that will take ssh keys when openstack server create is spawned with:
# --use-config-drive (--config-drive true)
# Example of userdata:
# #cloud-config
# password: core
# ssh_authorized_keys:
#   - ssh-rsa AAAAB(...)Jh0CpE+1xQ4ow==
#   - ssh-ed25519 AAAAC3(...)3dsAaY9
#   - ecdsa-sha2-nistp256 AAAAE2VjZ(...)uq5QC51C5+WWk=
#   - ssh-dss AAAyiM6qiF(...)iG8Wp4O73VUu2bg== someuser@host

CDROM=$(lsblk -fp | grep iso9660 | awk '{print $1}')
if [ -n "$CDROM" ]; then
    mkdir -p "$USERDATA_DIR"
    sudo mount "$CDROM" "$USERDATA_DIR" || true

    if [ -f "$USERDATA_DIR/openstack/latest/user_data" ]; then
        grep -E "^\s*- (ssh-rsa|ssh-ed25519|ecdsa-sha2-nistp256|ssh-dss)" "$USERDATA_DIR/openstack/latest/user_data" | sed 's/^\s*-\s*//' > "$USERDATA_KEY_DIR/userdata"
        if ssh-keygen -l -f "$USERDATA_KEY_DIR/userdata"; then
            cat "$USERDATA_KEY_DIR/userdata" | tee -a "${USER_DIR}/.ssh/authorized_keys.d/zuul"
        fi
    fi

    if [ -f "$USERDATA_DIR/openstack/latest/meta_data.json" ]; then
        cat "$USERDATA_DIR/openstack/latest/meta_data.json" | jq --raw-output '.public_keys[]'  > $USERDATA_KEY_DIR/userdata.raw
        grep -E "^(ssh-rsa|ssh-ed25519|ecdsa-sha2-nistp256|ssh-dss)" "$USERDATA_KEY_DIR/userdata.raw" > "$USERDATA_KEY_DIR/userdata"
        if ssh-keygen -l -f "$USERDATA_KEY_DIR/userdata"; then
            cat "$USERDATA_KEY_DIR/userdata" | tee -a "${USER_DIR}/.ssh/authorized_keys.d/zuul"
        fi
    fi
fi

# it will use key from: openstack server create --key-name <mykey>
CLOUD_INIT_KEYS=$(curl --connect-timeout 10 -SL "$CLOUD_INIT_KEYS_URL")
if [ -n "$CLOUD_INIT_KEYS" ]; then
    for k in $CLOUD_INIT_KEYS;
    do
        AVAILABLE_KEY=$(echo "$k" | cut -f1 -d"=")
        curl -SL "$CLOUD_INIT_KEYS_URL/$AVAILABLE_KEY/openssh-key" > "$USERDATA_KEY_DIR/openssh-key"

        if [ -f "$USERDATA_KEY_DIR/openssh-key" ]; then
            if ssh-keygen -l -f "$USERDATA_KEY_DIR/openssh-key"; then
                cat "$USERDATA_KEY_DIR/openssh-key" | tee -a "${USER_DIR}/.ssh/authorized_keys.d/zuul"
            fi
        fi
    done
fi
