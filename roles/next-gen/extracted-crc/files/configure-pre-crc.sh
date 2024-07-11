#!/bin/bash

set -x

HOST_IP=$(ip route get 1.2.3.4 | awk '{print $7}' | head -n1)
ALL_DNS=$(nmcli dev show | grep 'IP4.DNS'| awk '{print $2}')
DNS1=$(echo $ALL_DNS | sed -n '1 p')
DNS2=$(echo $ALL_DNS | sed -n '2 p')

if [ -z $DNS1 ]; then
    echo "DNS1 is empty. Setting 1.1.1.1"
    DNS1='1.1.1.1'
fi
if [ -z $DNS2 ]; then
    echo "DNS2 is empty. Setting 8.8.8.8"
    DNS2='8.8.8.8'
fi

if [ "$DNS1" == "$DNS2" ]; then
    DNS2='1.1.1.1'
fi

if [ -f /var/srv/dnsmasq.conf ]; then
    sed -i "s/192.168.130.11/$HOST_IP/g" /var/srv/dnsmasq.conf
fi

if [ -f /etc/dnsmasq.d/crc-dnsmasq.conf ]; then
    sed -i "s/192.168.130.11/$HOST_IP/g" /etc/dnsmasq.d/crc-dnsmasq.conf

    if grep -qv bind-interfaces /etc/dnsmasq.d/crc-dnsmasq.conf; then
cat << EOL | sudo tee -a /etc/dnsmasq.d/crc-dnsmasq.conf
# Add missing params
bind-interfaces
server=$DNS1
server=$DNS2
EOL
    systemctl restart dnsmasq
    fi
fi

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
