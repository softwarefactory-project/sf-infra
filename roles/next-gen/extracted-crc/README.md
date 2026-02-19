# Extracted CRC role

## Main goal

The main goal of this role is to:

- trigger CRC bundle generate script,
- extract the bundle file,
- mount the CoreOS qcow2 image,
- modify the `crc.qcow2` image by injecting:

  - custom SSH key like Zuul CI ssh pub keys,
  - set custom DNS ip address,

- umount the qcow2 image,
- pull the image on the host which has OpenStack credentials,
- push the image to the OpenStack cloud project,
- remove old image and rename new one to main name (*)

(*) This step is necessary due to the Zuul CI nodepool workflow. When
a VM is first spawned with an image, nodepool caches the image UUID to
speed up future requests. The problem arises because the image is not
created through Nodepool; it’s a cloud image. Later, Nodepool uses the
image name to spawn instances, but it keeps the cached UUID (X) even
after a new image is uploaded. Nodepool does not check if the image
name corresponds to a new UUID, so after each upload, we must either
clear the cache and restart the Nodepool launcher service, or remove
the old image and trigger the service to cache the new
one. Unfortunately, we cannot change the UUID of the old image.

## Basic workflow by using shell

Similar result you will have after executing below bash script:

```sh
#!/bin/bash

set -x

### NOTE: Remember to run as a root ;)

sudo mkdir -p /mnt/extracted
sudo guestmount -m /dev/sda4 -a crc.qcow2 --rw /mnt/extracted
GUEST_ETC_DIR="$(sudo find /mnt/extracted/ostree/deploy/rhcos/deploy -maxdepth 1 -type d | tail -n1)/etc"
GUEST_LOCAL_DIR=/mnt/extracted/ostree/deploy/rhcos/var/usrlocal
GUEST_HOME_DIR=/mnt/extracted/ostree/deploy/rhcos/var/home/core

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
# zuul gpc spare
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC+zxUhIIscUPLAbF2CP19dyeVpkbKGL076EHBvHuiKISru5NRv9au+XWvUfSCkpuDO6Brq1Y15Mq+cvc+/aHPxTk5wkeXNwgsw2vigsIwAwG1pHT9v08lnYyXvZS7QYWbzIUgxfJqQhYaNKUPqbR3v1+JJnMxQPQuly7jqFf7K7JYLh/uPWtXfp9oNlmnk+qdZhlUZoKxoP8lK9O2eJH2bYMsTurIhZ99gDc0yMvwPqIGI5G1ko9cPKInSiXCAZNr/OkrUQcbvZQFUF1Y/JfRTUnn8k6JMRKM+I4A5y/ONWNZJlGUSNPJBTcaMISjLr6j4VhkhrSgW4XYaVlR4uRPyczf4DZXo5mITyTCY2cAl3zDWGJ1ana9YV8UNQMzQVLF/EzKsMFg5Nb5B5FPARqgS+6D5rh3CRALn3/yI2r/rfCQZe95ISXwv3yTFmoadSdKdJC14PbTAIFsbpnyYCmlIjH+aipghsQeFjTfHuFNjNI8lI+Rjo82zluZ840rJ4yYedoqCJ7t3hZ9ip5XSJKcVVp9Et2xVt+o6wYJLzMw2FNrQK57TMqbAlPPve+O4o03CCRVj376/+5QT1yDWsrdCCAUXolhHGMt5eODJdFEWPFFVIdyBQdX+eD+ZevoM/fuKEjcLs+cSQO65GE9OW3mmUfsjqm6jtlFPLzhkwVW07Q==
# zuul gpc
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCqgwhKyRlrqsbllwkFy/9aZ/IEsadMkhFBDHN+GQN+a0tbTNZR9jBvM+SJBvSXE8fFyDFLPDAcsiqe+Q1Qi9dnZtCR19SJCIzVV0+6z6Lp5Tep+xV1iJRlZwB4I54Zk0Xc2yRSi9dIURNe9MRb4mEIXVagOxraIkuDw9fovPA32vMDqCSmwW8InlHpxvF1bU3BzDTBMndTxasvQc6Kzak9xAkWrMyPX8B6lzTpJivjiVapPgS1065smFrfmm1sDYFlQGPyG3dtRqQh5DWBPkO4chEUxTOKvflAhBRg6X3dfWbRVrC8UpgOOIbD0+u8lbBpRA/97yyr8V+C6RzdeR3p6ptpMon7zlm/VvyBbwzrOPRAJSICFXWoKQHFtGj4710peZ5ASevPIm5ih3ln1DRIlZuQBz4Ir+gS0WcPNz2kwvxWawJQZJKsHPxqUL9NdQFqIN0YJAlG2t5hLi86xS+85rU5ICe+y8pVnqn52i3Z7sUzu0ADaqndSdPcoaGsQ3l+zwrgS/KXkV2nnoZuOvQDOOr/MiUcEEVLz6JUArrLA5YiUYHARoKGwk+CP+LCm8DvYK3DOlioCVt+fwyPUT5wD7c7A29mL8TJlENgMBgAKkty4cRDCurIfKxG4CL9Z2/w5C1SFPFroA6pU2YDMCX1hbB7hBihguwa3oUTARSDsQ==
# image-builder host - for re-creating CRC image
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIppNzjGx/3nYeinqak/o9UYgtC42kymxv/s3SxVXbJ3 centos@image-builder
EOF

sync
sudo guestunmount /mnt/extracted
```
