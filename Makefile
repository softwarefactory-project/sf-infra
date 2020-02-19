MANAGED = playbooks/vars/infra-sf.yaml \
	  playbooks/vars/nodepool-sf.yaml \
	  playbooks/vars/nodepool-tripleo.yaml \
	  playbooks/vars/nodepool-rdo.yaml \
	  ansible/hosts.yaml

all: dhall-schemas dhall-format $(MANAGED)
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
