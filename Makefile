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
	  playbooks/host_vars/prometheus.monitoring.softwarefactory-project.io.yaml \
	  roles/acme-tiny/tasks/main.yaml \
	  roles/acme-tiny/defaults/main.yaml

ANSIDHALL = roles/acme-tiny/tasks/main.yaml

all: dhall-version-check dhall-format $(MANAGED) prettify-generated-ansible
	@dhall to-directory-tree --output . <<< ./vars/directory-tree.dhall

.FORCE:
%.yaml: %.dhall .FORCE
	@sh -c "echo '# This file is managed by dhall.'; env DHALL_INFRA=$$(pwd)/package.dhall dhall-to-yaml --explain --file $<" > $@

prettify-generated-ansible:
	@python3 scripts/yaml-prettifier.py roles/acme-tiny/tasks/main.yaml
	@dhall to-directory-tree --output roles/acme-tiny/templates <<< "(./roles/acme-tiny/role.dhall).Templates"
	@dhall text > roles/acme-tiny/README.md                     <<< "(./roles/acme-tiny/role.dhall).README"

dhall-inventory:
	@dhall-to-json --file ansible/hosts.dhall | python3 scripts/gen_inventory.py | json-to-dhall | dhall-to-yaml > ansible/hosts.yaml

dhall-format:
	@find . -name "*.dhall" -exec dhall --ascii format --inplace {} \;

DHALL_PACKAGE = https://copr-be.cloud.fedoraproject.org/results/tdecacqu/dhall/fedora-rawhide-x86_64/01513235-dhall/dhall-1.33.1-1.fc33.x86_64.rpm
dhall-version-check:
	@sh -c 'test 103299 -lt $$(dhall --version | sed "s/\./0/g") || (echo -e "You need dhall version > 1.33.0, please update by running:\n sudo dnf install -y $(DHALL_PACKAGE)"; exit 1)'
