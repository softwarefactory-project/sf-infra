#!/bin/bash

set -x

# NOTE:(dpawlik) Added a parameter to stop making a DNS request storm
# for getting api-int.crc.testing domain.
HOST_IP=$(ip route get 1.2.3.4 | awk '{print $7}' | head -n1)

if [ -f /var/srv/dnsmasq.conf ]; then
    sed -i "s/192.168.130.11/$HOST_IP/g" /var/srv/dnsmasq.conf
fi

if [ -f /etc/dnsmasq.d/crc-dnsmasq.conf ]; then
    sed -i "s/192.168.130.11/$HOST_IP/g" /etc/dnsmasq.d/crc-dnsmasq.conf
fi

systemctl restart dnsmasq
