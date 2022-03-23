{-|
# This directory contains function to generate common prometheus configurations

The yaml files are generated by the Makefile from the MANAGED list.
The main configuration is constructed with the *prometheus-config.dhall* .-}
{ binding = ./binding.dhall
, demo = ./demo.dhall
, prometheus-config = ./prometheus-config.dhall
, prometheus = ./prometheus.dhall
, rules-afs = ./rules-afs.dhall
, rules-backup = ./rules-backup.dhall
, rules-dlrn = ./rules-dlrn.dhall
, rules-http = ./rules-http.dhall
, rules-mysqld = ./rules-mysqld.dhall
, rules-node = ./rules-node.dhall
, rules-node_proxy = ./rules-node_proxy.dhall
, rules-nodepool = ./rules-nodepool.dhall
, rules-openstack-check = ./rules-openstack-check.dhall
, rules-systemd = ./rules-systemd.dhall
, rules-zuul = ./rules-zuul.dhall
, rules = ./rules.dhall
, scrape-configs = ./scrape-configs.dhall
}