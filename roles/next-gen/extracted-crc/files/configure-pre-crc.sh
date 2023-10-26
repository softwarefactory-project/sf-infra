#!/bin/bash

set -x

HOST_IP=$(ip route get 1.2.3.4 | awk '{print $7}' | head -n1)
HOST_DEF_INT=$(ip -br -4 a sh | grep $HOST_IP | awk '{print $1}')
ALL_DNS=$(nmcli -g IP4.DNS connection show $(nmcli connection show | grep $HOST_DEF_INT | awk -F '  ' '{print $2}'))
DNS1=$(echo $ALL_DNS| cut -f1 -d'|')
DNS2=$(echo $ALL_DNS| cut -f2 -d'|')
USER_DIR=${USER_DIR:-'/var/home/core'}
CLOUD_INIT_KEYS_URL=${CLOUD_INIT_KEYS_URL:-'http://169.254.169.254/latest/meta-data/public-keys'}
USERDATA_DIR=${USERDATA_DIR:-'/tmp/openstack-config-drive'}
USERDATA_KEY_DIR=${USERDATA_KEY_DIR:-'/tmp/ssh-pub-keys'}

if [ -z $DNS1 ]; then
    echo "DNS1 is empty. Setting 1.1.1.1"
    DNS1='1.1.1.1'
fi
if [ -z $DNS2 ]; then
    echo "DNS2 is empty. Setting 8.8.8.8"
    DNS2='8.8.8.8'
fi

cat << EOL | sudo tee /var/srv/dnsmasq.conf
user=root
port= 53
bind-interfaces
expand-hosts
log-queries
local=/crc.testing/
domain=crc.testing
address=/apps-crc.testing/$HOST_IP
address=/api.crc.testing/$HOST_IP
address=/api-int.crc.testing/$HOST_IP
address=/crc-74q6p-master-0.crc.testing/192.168.126.11
EOL

cat << EOL | sudo tee /etc/resolv.conf
nameserver $HOST_IP
nameserver $DNS1
nameserver $DNS2
EOL

# stop overwriting /etc/resolv.conf after reboot
cat << EOL | sudo tee /etc/NetworkManager/conf.d/00-custom-crc.conf
[main]
dns=none
EOL

sudo systemctl reload NetworkManager

# inject alternative SSH keys
mkdir -p "$USERDATA_KEY_DIR" && cd "$USERDATA_KEY_DIR"

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
    sudo mount "$CDROM" "$USERDATA_DIR"

    if [ -f "$USERDATA_DIR/openstack/latest/user_data" ]; then
        grep -E "^\s*- (ssh-rsa|ssh-ed25519|ecdsa-sha2-nistp256|ssh-dss)" "$USERDATA_DIR/openstack/latest/user_data" | sed 's/^\s*-\s*//' > "$USERDATA_KEY_DIR/userdata"
        cat "$USERDATA_KEY_DIR/userdata" | tee -a "${USER_DIR}/.ssh/authorized_keys"
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
            if grep -qE "(ssh-rsa|ssh-ed25519|ecdsa-sha2-nistp256|ssh-dss)" "$USERDATA_KEY_DIR/openssh-key"; then
                cat "$USERDATA_KEY_DIR/openssh-key" | tee -a "${USER_DIR}/.ssh/authorized_keys"
            fi
        fi
    done
fi
