SSLCertExpiringSoon
===================

Why is it firing ?
------------------

A SSL certificate is about to expire.

Solution
--------

On our infra we have two solutions for renewing SSL certs.
To choose correct one, ssh into the host and check the crontab entries,
or just check if "certbot" package is installed via simply command:
`rpm -qa |grep certbot`.

Available methods:

#### Acme-tiny

* ssh into the machine
* as root: `/usr/local/bin/acme-tiny-refresh.sh no-reload`
* ensure the certificate is correctly renewed: `openssl x509 -noout -dates -in /etc/letsencrypt/pem/$(hostname).pem`

```
> **NOTE**:
It can be a situation, when earlier was a `lecm` script, where the destination
directories was different than in acme-tiny-refresh script. In that case,
make sure that `SSLCertificateChainFile` param in Apache2 vhost configuration file
is set to be same as `SSLCertificateFile` (this file include cert and chain).
```
```
> **WARNING**:

It can happen, that due changing the sytem from `lecm` to `acme-tiny-refresh`
script:

    - the script will fail to refresh the certs and the Apache2 is down:

        * comment section `*:443` in Apache2 vhost configuration file
        * restart `httpd` service
        * run `acme-tiny-refresh.sh` script
        * uncomment section `*:443` in Apache2 vhost configuration file

    - Apache2 error: `hostname in certificate didn't match`

        * do same procedure as in point above (yes, refresh SSL cert)
        * ensure that the vhost parameter: `SSLCertificateChainFile` path is correct
```

#### Certbot
* ssh into the machine
* as root: `/usr/bin/certbot certonly -a webroot --keep-until-expiring -d $(hostname) --webroot-path /var/www/html && /usr/bin/systemctl reload httpd`
* ensure the certificate is correctly renewed: `openssl x509 -noout -dates -in /etc/letsencrypt/pem/$(hostname).pem`
