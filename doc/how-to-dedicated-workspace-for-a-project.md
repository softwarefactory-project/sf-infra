# Dedicated workspace for a project

If you need a dedicated ansible.cfg and inventory, it's possible to create a
dedicated directory (eg. my_project) for the files, roles and playbooks needed
for yours needs.

Two playbooks should be created, the first one to launch the zuul job, the
second one with the configuration you want to apply. The first playbook (eg.
my_project/run.yaml) should contains 2 steps. The first step to add the bridge
host, the second part contain the action executed from the bridge:

```my_project/run.yaml
- hosts: localhost
  vars:
    bridge_name: bridge.softwarefactory-project.io
    bridge_ip: 38.102.83.244
    bridge_key: "{{ bridge_name }},{{ bridge_ip }},bridge ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG+w4PGQtn2VpFwEpww7uYaVcubHMvKCiM1uj6NOwx9X"
  tasks:
    - add_host:
        name: "{{ bridge_name }}"
        ansible_python_interpreter: python3
        ansible_host: "{{ bridge_ip }}"
        ansible_user: fedora
        ansible_connection: ssh
        groups: bridge

    - known_hosts:
        name: "{{ bridge_name }}"
        key: "{{ bridge_key }}"

- hosts: bridge.softwarefactory-project.io
  gather_facts: no
  tasks:
    - block:
        - name: Start zuul_console daemon.
          zuul_console:

        - name: Synchronize src repos to workspace directory.
          synchronize:
            dest: "~/src/"
            src: "{{ zuul.executor.src_root }}/"
          no_log: true

        - name: Run configuration playbook
          args:
            chdir: "~/{{ zuul.project.src_dir }}"
          command: "ansible-playbook -i my_project/ansible/hosts.yaml -vv my_project/playbooks/deploy.yaml"
          environment:
            ANSIBLE_CONFIG: my_project/ansible/ansible.cfg
```

Then, add your job definition, and add the job in `project/check` and
`project/gate` sections. The `files` part is required, to ensure your job will be run
only if files in my_project are changed:

```zuul.d/jobs.yaml
- job:
    name: manage_my_project
    parent: sf-infra-base
    description: Configure my_project
    semaphore: sf-infra
    files:
      - ^my_project/*$
    run: my_project/run.yaml
```
