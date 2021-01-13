MariaDBDown
===========

Why is it firing ?
------------------
Probably MariaDB service is down.

To check
--------
- check if mariadb-server service is running
- check if mysql_exporter service is working
- check if connection between prometheus and host for mysql_exporter (port 9104)
