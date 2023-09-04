#!/bin/bash
HOST_IP=$(ip route get 1.2.3.4 | awk '{print $7}' | head -n1)
HOST_DEF_INT=$(ip -br -4 a sh | grep $HOST_IP | awk '{print $1}')
ALL_DNS=$(nmcli -g IP4.DNS connection show $(nmcli connection show | grep $HOST_DEF_INT | awk -F '  ' '{print $2}'))
DNS1=$(echo $ALL_DNS| cut -f1 -d'|')
DNS2=$(echo $ALL_DNS| cut -f2 -d'|')

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
