\(account-file : Text) ->
\(csr-file : Text) ->
\(cert-file : Text) ->
\(challenge-dir : Text) ->
  ''
  #!/bin/sh
  RELOAD=0

  {% for item in acme_domains %}
  /sbin/acme-tiny --account-key ${account-file} \
                  --csr ${csr-file}             \
                  --acme-dir ${challenge-dir}   \
                  > ${cert-file}.tmp
  if [ $? == 0 ] && [ -f ${cert-file}.tmp ]; then
      mv ${cert-file}.tmp ${cert-file}
      RELOAD=1
  fi
  {% endfor %}

  if [ $RELOAD == 1 ]; then
     systemctl reload httpd
     # TODO: check if service is running, otherwise restore previous files?
  fi
  ''
