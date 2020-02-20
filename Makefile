MANAGED = playbooks/vars/infra-sf.yaml \
	  playbooks/vars/nodepool-sf.yaml \
	  playbooks/vars/nodepool-tripleo.yaml \
	  playbooks/vars/nodepool-rdo.yaml \
	  ansible/hosts.yaml

all: dhall-version-check dhall-schemas dhall-format $(MANAGED)
	@dhall to-directory-tree --output . <<< ./conf/tree.dhall

%.yaml: %.dhall
	@sh -c "echo '# This file is managed by dhall.'; dhall-to-yaml --explain --file $<" > $@

# dhall-schemas generate the schemas.dhall file from the schemas directory content
dhall-schemas:
	@python3 conf/scripts/gen_groups.py > conf/types/Groups.dhall
	@python3 -c 'import os; print("{ " + \
	" , ".join(map(lambda x: x.replace(".dhall", "") + " = ./schemas/" + x, sorted(os.listdir("conf/schemas")))) + \
	" }")' > conf/schemas.dhall

dhall-format:
	@find . -name "*.dhall" -exec dhall --ascii format --inplace {} \;

DHALL_PACKAGE = https://download.copr.fedorainfracloud.org/results/tdecacqu/dhall/fedora-rawhide-x86_64/01246620-dhall/dhall-1.30.0-1.fc33.x86_64.rpm
dhall-version-check:
	@sh -c 'test -z "$$(dhall --version | grep ^1.2)" || (echo -e "You need dhall version > 1.29, please update by running:\n sudo dnf install -y $(DHALL_PACKAGE)"; exit 1)'
