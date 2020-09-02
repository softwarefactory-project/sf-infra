\(raw-domain : Text) ->
\(domain : Text) ->
\(challenges-dir : Text) ->
\(vhost_root : Text) ->
\(cert-file : Text) ->
\(key-file : Text) ->
  ''
  <VirtualHost *:80>
      ServerName ${domain}
      Alias /.well-known/acme-challenge ${challenges-dir}/${domain}
      RewriteEngine On
      RewriteCond %{HTTPS} off
      RewriteCond %{REMOTE_HOST} !${domain}$
      RewriteCond %{REQUEST_URI} !\.well-known/acme-challenge
      RewriteRule (.*) https://${domain}%{REQUEST_URI} [R=301,L]

      CustomLog "logs/${domain}_access_log" combined
      ErrorLog "logs/${domain}_error_log"
  </VirtualHost>

  {% for cert_files_result in _cert_files["results"] %}
  {% if cert_files_result.stat.exists and cert_files_result["item"]["domain"] == ${raw-domain} %}
  <VirtualHost *:443>
      ServerName ${domain}
      Alias / ${vhost_root}

      SSLEngine on
      SSLCertificateFile      ${cert-file}
                              # TODO: check where this come from and install it
      SSLCertificateChainFile /etc/letsencrypt/pem/lets-encrypt-x3-cross-signed.pem
      SSLCertificateKeyFile   ${key-file}

      CustomLog "logs/${domain}_access_log" combined
      ErrorLog "logs/${domain}_error_log"
  </VirtualHost>
  {% endif %}{% endfor %}
  ''
