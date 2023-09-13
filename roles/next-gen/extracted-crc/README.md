# Extracted CRC role

## Main goal

The main goal of that role is to:

- trigger CRC bundle generate script,
- extract the bundle file,
- mount the CoreOS qcow2 image,
- modify the `crc.qcow2` image by injecting:

  - custom SSH key like Zuul CI ssh pub keys,
  - set custom DNS ip address,

- umount the qcow2 image,
- pull the image on the host which have OpenStack credentials,
- push the image to the OpenStack cloud project,
- remove old image and rename new one to main name*

* We are doing that step because of the Zuul CI nodepool workflow. After
first spawn of VM with such image, nodepool is caching the image UUID to speed-up
future request. That might be a problem for the CI, due the image is not created
via Nodepool, but it is cloud image, which later Nodepool is taking that name
to spawn an instance. In other words, the Nodepool launcher keeps in cache
old image with uuid `X` and after we upload new image, it still keeps in cache
uuid `X` without attepting to check if there is another image with such name.
It means, that after each upload, we should clear cache and restart Nodepool
launcher service or remove old image and trigger service to take the new image
to the cache (because we can not change the old image UUID to be different).

## Basic workflow by using shell

Similar result you will have after executing below sh script:

```sh
#!/bin/bash

set -x

### NOTE: Remember to run as a root ;)

sudo guestmount -m /dev/sda4 -a crc.qcow2 --rw /mnt
GUEST_ETC_DIR="$(sudo find /mnt/ostree/deploy/rhcos/deploy -maxdepth 1 -type d | tail -n1)/etc"
GUEST_LOCAL_DIR=/mnt/ostree/deploy/rhcos/var/usrlocal
GUEST_HOME_DIR=/mnt/ostree/deploy/rhcos/var/home/core

cat << EOF > $GUEST_ETC_DIR/systemd/system/crc-pre.service
[Unit]
Description=Configure required service configuration files
DefaultDependencies=no
ConditionPathExists=/usr/local/bin/configure-pre-crc.sh
Before=kubelet.service crc-dnsmasq.service
After=NetworkManager.service network-online.target
Wants=network-online.target

[Service]
ExecStart=bash /usr/local/bin/configure-pre-crc.sh
Type=oneshot
TimeoutSec=60
RemainAfterExit=yes

[Install]
WantedBy=kubelet.service
EOF

cat << 'EOF' > $GUEST_LOCAL_DIR/bin/configure-pre-crc.sh
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
# NOTE: Other DNS nameservers are set in /var/srv/dnsmasq.conf
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
EOF

# set execute for others
sudo chmod +x $GUEST_LOCAL_DIR/bin/configure-pre-crc.sh

# enable service
cd $GUEST_ETC_DIR/systemd/system/default.target.wants
ln -s ../crc-pre.service .
cd -

cat << 'EOF' >> $GUEST_HOME_DIR/.ssh/authorized_keys
# https://github.com/rdo-infra/review.rdoproject.org-config/blob/master/playbooks/tripleo-rdo-base/pre.yaml
# zuul - important
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC7ToRkLRS8QHhlMYvqUl2GF5nGI1DkOAbHJv7b9ru608ISb7+hkUpposBclCGnjR3Bvtbsj3kcJOErZrczR2WfJ52JwjaWTSXU8vh20l3aVtHtPRdlLGCHfM92Ll85eA6FrtZB4UaVkabqFiA1GmTUUg4q9W9wxngwMWdI5ZcifZ+0uXjDLFazVtNGyTNVyWi0b2ECFBCGFV2Bg6FI9Y+QWosNYvhs+jRzPMdtat2fwbq+2p5kU8SkWFZYAva2Anrbv5T5cGhT0K7cO5dEAWSUdVOSTvnv5JAoygdrTtDJOP4cKqtL6+dPdW3nZbjqg0fCUe1feWpknu3L2GHaOzidhsG+RUDQob0NXFmyej4JmLF95x3XVVgJPzMEAVBqI83ev8GcPv1Lb75jkqsolV9TzKP2sB26MV1aCRluZzibOpjQvzKzvA0sjR+5PG9iJ1KwcSg6MfukEE7WuZ0hEhfmt/nH3ZOQv6mi3cKRYXRfK2jJ7lVMutaD02hZxN7ppd8= zuul-build-sshkey
# zuul zs.sf.io
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDgOp/WpwK0HBfgJoYDXo+yZbLqkNrbkbtk3p6MxJumQlXxXhKWY0uJXbrecjhJG6Ydv/6SzKQeoVWSPJUkZ0xD1l7KCPN+iNJyRQGwiyIi/Vd7JNt0pn+dblmPA5GzAwEMcT+49OlK0I1p1JwpSa0CFgNH8zSZOqCaH8yUiKtbc0UtCdQehSIcHvz573E2IbeDMG1omijf6fAT67tEAzEbsasCN/bSmXDraAQ+XIPPsFoifCQaSOL3SsyjG0awNfTotiBW68DqzR29KYwMQntM1ACVWPMda2rVTUFmV51ono/Ux2vRiV8zMCQAzZdy9gkF+3bDnK7VeY2rccp7EaVj root@managesf
# zuul sfhosted
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDU84vH1QOZ8G4v3big/1Eh7ErtQyAXCC2YyqBtaCoDZFmfQHFqOeBdHaR3heRZYo9MCl9wiAtqXWYGmOMndbYooB09LCzA0DXtBRJtLeRYPk3jlfYI6hd1VhMipnzVlAPfmNfFBCujROdUT2gaO/cOGREWrsfTDYjYpHt7ltDZ+XwdB3vFqW/K9mgJz27VTnGSn/4J6LDai36FkXQEs05gcKbFMC5HOHNrxqpl4obLASB5K/XV18Ret43mIbHtFaLhjBRFl97kBkohQK/IGG+WVccjA/5Y84AOUk53WIHz07B6qgybl8wd3nAj9dXl5fPMyunF5Eb9ev1IBWR2idtj root@sf.hosted
EOF

sync
sudo guestunmount /mnt
```
