Role to set up firewalld and rules
==================================

This role will enable firewalld and open the required ports.

# Variables

The role will use the following variable, defined in the inventory:

* `firewall_rules` lists the firewall rules to apply. You need to specify
  the list in the following format:

        firewall_rules:
            - port: '3306/tcp'
              permanent: yes (optional)
              immediate: yes (optional)
              state: enabled (optional)
            - port: '9100/tcp'
              permanent: yes (optional)
              immediate: yes (optional)
              state: enabled (optional)
