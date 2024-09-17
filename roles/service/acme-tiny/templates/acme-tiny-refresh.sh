#!/bin/bash

export PATH=/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin

RELOAD=1

{% for item in acme_domains %}

acme-tiny --account-key {{ acme_keys_dir }}/account.key \
                --csr {{ acme_keys_dir }}/{{ item.domain }}.csr \
                --acme-dir {{ acme_challenges_dir }}/{{ item.domain }} \
                > {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp
if [ $? == 0 ] && [ -f {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp ]; then
    mv {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp {{ acme_certs_dir }}/{{ item.domain }}.pem
    RELOAD=0
fi
sleep 5
{% endfor %}

if [ $RELOAD -eq 0 ]; then
   systemctl reload httpd
   # TODO: check if service is running, otherwise restore previous files?
fi
