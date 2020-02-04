all: dhall-format
	sh -c "echo '# this file is managed by dhall'; dhall-to-yaml --explain --file playbooks/vars/infra-sf.dhall" > playbooks/vars/infra-sf.yaml
	sh -c "echo '# this file is managed by dhall'; dhall-to-yaml --explain --file playbooks/vars/infra-rdo.dhall" > playbooks/vars/infra-rdo.yaml

dhall-format:
	@find . -name "*.dhall" -exec dhall --ascii format --inplace {} \;
