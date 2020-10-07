SSLCertExpiringSoon
===================

Why is it firing ?
------------------

A SSL certificate is about to expire.

Solution
--------

* ssh into the machine
* as root: `PATH=$PATH:/usr/sbin /usr/bin/lecm --renew`
* ensure the certificate is correctly renewed: `PATH=$PATH:/usr/sbin /usr/bin/lecm --list-details`
