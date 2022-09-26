Role to set up a MariaDB instance with replication
==================================================

This role can be used to setup a replicated MariaDB instance, with a master and
a replica. Replication will always happen via SSL.

# Server groups

The following server groups must be defined in the inventory:

* `db_master`: The host in this group will be the master.
* `db_replica`: The host(s) in this group will be the replica(s).

# Variables

The role will use the following variables, defined in the inventory:

* `db_repl_user` is the database user that will be created to run the
   replication.
* `db_password` is the password used for the root account in the databases, and
  also for the replication user.
* `db_ssl_certs` will define the different SSL certificates. The following
  structure is needed:

        db_ssl_certs:
          ca-cert:
            filename: "ca-cert.pem"
            contents: |
                -----BEGIN CERTIFICATE-----
                <certificate contents>


  Each entry will have a name, and two keys: `filename` to specify the file
  name, and `contents` with the certificate contents. All certificates included
  here will be stored under `/etc/mysql/ssl`.

# Usage

The role contains playbooks for both master and replica, so you will need to
include the specific task file. To set up a master:

    ---
    - hosts: db_master
      tasks:
        - include_role:
            name: mariadb-replicated
            tasks_from: master.yml


To set up a replica, you will need to do so in the same playbook as the master,
because some variables need to be set by it.

    ---
    - hosts: db_replica
      tasks:
        - include_role:
            name: mariadb-replicated
            tasks_from: replica.yml

