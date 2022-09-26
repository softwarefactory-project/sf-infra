SHELL = /bin/bash

MANAGED = playbooks/vars/infra-sf.yaml \
	  playbooks/vars/infra-rdo.yaml \
	  playbooks/vars/nodepool-sf.yaml \
	  playbooks/vars/nodepool-tripleo.yaml \
	  playbooks/vars/nodepool-rdo.yaml \
	  monitoring/prometheus.yaml \
	  monitoring/rules-afs.yaml \
	  monitoring/rules-backup.yaml \
	  monitoring/rules-dlrn.yaml \
	  monitoring/rules-http.yaml \
	  monitoring/rules-node.yaml \
	  monitoring/rules-node_proxy.yaml \
	  monitoring/rules-nodepool.yaml \
	  monitoring/rules-systemd.yaml \
	  monitoring/rules-zuul.yaml \
	  monitoring/rules-mysqld.yaml \
	  monitoring/rules-openstack-check.yaml \
	  monitoring/rules-es-check.yaml \
	  monitoring/rules-system-package-count-sf.dhall \
	  monitoring/rules-system-package-count-rdo.dhall \
	  playbooks/host_vars/prometheus.monitoring.softwarefactory-project.io.yaml \
	  playbooks/host_vars/backup.rdoproject.org.yaml

DASHBOARDS = roles/sf/grafana/files/DLRN.json

ANSIDHALL = roles/acme-tiny/tasks/main.yaml

all: dhall-version-check dhall-format dhall-inventory $(DASHBOARDS) $(MANAGED)
	@dhall to-directory-tree --output . <<< ./vars/directory-tree.dhall

%.json: %.dhall .FORCE
	@dhall-to-json --explain --file $< --output $@

.FORCE:
%.yaml: %.dhall .FORCE
	@sh -c "echo '# Code generated by dhall-to-yaml.  DO NOT EDIT.'; env DHALL_INFRA=$$(pwd)/package.dhall dhall-to-yaml --explain --file $<" > $@

dhall-inventory:
	@dhall-to-json --file ansible/hosts.dhall | python3 scripts/gen_inventory.py | json-to-dhall | dhall-to-yaml > ansible/hosts.yaml

dhall-format:
	@find . -name "*.dhall" -exec dhall --ascii format --inplace {} \;

dhall-version-check:
	@sh -c 'test 103199 -lt $$(dhall --version | sed "s/\./0/g") || (echo -e "You need dhall version >= 1.32.0, please update by running:\n sudo dnf install -y dhall dhall-json"; exit 1)'
