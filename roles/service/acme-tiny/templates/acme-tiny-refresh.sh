#!/bin/sh

RELOAD=$1

NODELAY=$2

if [ "NODELAY" != "no-delay" ]; then
  # Avoid the high spike crippling let's encrypt servers at XX:00
  DELAY=$((($RANDOM%900) + 900))
  sleep $DELAY
fi

{% for item in acme_domains %}

$(which acme-tiny) --account-key {{ acme_keys_dir }}/account.key \
                --csr {{ acme_keys_dir }}/{{ item.domain }}.csr             \
                --acme-dir {{ acme_challenges_dir }}/{{ item.domain }}   \
                > {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp
if [ $? == 0 ] && [ -f {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp ]; then
    mv {{ acme_certs_dir }}/{{ item.domain }}.pem.tmp {{ acme_certs_dir }}/{{ item.domain }}.pem
    RELOAD=1
fi
sleep 5
{% endfor %}

if [ $RELOAD -eq 1 ] && [ "$RELOAD" != 'no-reload' ]; then
   systemctl reload httpd
   # TODO: check if service is running, otherwise restore previous files?
fi
