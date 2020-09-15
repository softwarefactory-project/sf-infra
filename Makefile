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
	  playbooks/host_vars/prometheus.monitoring.softwarefactory-project.io.yaml \
	  roles/acme-tiny/tasks/main.yaml \
	  roles/acme-tiny/defaults/main.yaml \
	  ansible/hosts.yaml

ANSIDHALL = roles/acme-tiny/tasks/main.yaml

all: dhall-version-check dhall-schemas dhall-format $(MANAGED) prettify-generated-ansible
	@dhall to-directory-tree --output . <<< ./vars/directory-tree.dhall

.FORCE:
%.yaml: %.dhall .FORCE
	@sh -c "echo '# This file is managed by dhall.'; env DHALL_INFRA=$$(pwd)/package.dhall dhall-to-yaml --explain --file $<" > $@

prettify-generated-ansible:
	@python3 conf/scripts/yaml-prettifier.py roles/acme-tiny/tasks/main.yaml
	@dhall to-directory-tree --output roles/acme-tiny/templates <<< "(./roles/acme-tiny/role.dhall).Templates"
	@dhall text > roles/acme-tiny/README.md                     <<< "(./roles/acme-tiny/role.dhall).README"

# dhall-schemas generate the package files from diretory content
dhall-schemas:
	@python3 conf/scripts/gen_groups.py  conf/types/Group.dhall > conf/types/Groups.dhall
	@python3 conf/scripts/gen_package.py conf/schemas > conf/schemas.dhall
	@python3 conf/scripts/gen_list.py vars/*/instances.dhall > vars/instances.dhall

dhall-format:
	@find . -name "*.dhall" -exec dhall --ascii format --inplace {} \;

DHALL_PACKAGE = https://copr-be.cloud.fedoraproject.org/results/tdecacqu/dhall/fedora-rawhide-x86_64/01513235-dhall/dhall-1.33.1-1.fc33.x86_64.rpm
dhall-version-check:
	@sh -c 'test 103299 -lt $$(dhall --version | sed "s/\./0/g") || (echo -e "You need dhall version > 1.33.0, please update by running:\n sudo dnf install -y $(DHALL_PACKAGE)"; exit 1)'
