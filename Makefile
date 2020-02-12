all: dhall-schemas dhall-format
	@sh -c "echo '# this file is managed by dhall'; dhall-to-yaml --explain --file playbooks/vars/infra-sf.dhall" > playbooks/vars/infra-sf.yaml
	@sh -c "echo '# this file is managed by dhall'; dhall-to-yaml --explain --file playbooks/vars/infra-rdo.dhall" > playbooks/vars/infra-rdo.yaml
	@sh -c "echo '# this file is managed by dhall'; dhall-to-yaml --explain --file playbooks/vars/nodepool-sf.dhall" > playbooks/vars/nodepool-sf.yaml
	@sh -c "echo '# this file is managed by dhall'; dhall-to-yaml --explain --file playbooks/vars/nodepool-tripleo.dhall" > playbooks/vars/nodepool-tripleo.yaml

# dhall-schemas generate the schemas.dhall file from the schemas directory content
dhall-schemas:
	@python3 -c 'import os; print("{ " + \
	" , ".join(map(lambda x: x.replace(".dhall", "") + " = ./schemas/" + x, sorted(os.listdir("conf/schemas")))) + \
	" }")' > conf/schemas.dhall

dhall-format:
	@find . -name "*.dhall" -exec dhall --ascii format --inplace {} \;
