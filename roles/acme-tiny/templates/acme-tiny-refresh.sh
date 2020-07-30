#!/bin/sh
RELOAD=0

{% for item in acme_domains %}
/sbin/acme-tiny --account-key {{ acme_keys_dir }}/account.key \
                --csr {{ acme_keys_dir }}/{{ item.domain }}.csr             \
                --acme-dir {{ acme_challenges_dir }}/{{ item.domain }}   \
                > {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp
if [ $? == 0 ] && [ -f {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp ]; then
    mv {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp {{ acme_certs_dir }}/{{ item.domain }}.pem
    RELOAD=1
fi
{% endfor %}

if [ $RELOAD == 1 ]; then
   systemctl reload httpd
   # TODO: check if service is running, otherwise restore previous files?
fi
