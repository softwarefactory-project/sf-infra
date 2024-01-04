let Infra = ../Infra/package.dhall

let {- special case for openshift-ansible that requires a group of groups -}
    openshift-ansible-group =
      { OSEv3 = [ "etcd", "masters", "nodes", "installer" ] }

let {- special case for OSPO that requires a group of groups -}
    ospo-ansible-group =
      { osci_zone = [ "osci_internal_zone" ] }

let instances = Infra.Instance.keepPresent ../vars/instances.dhall

in  { groups =
          Infra.AnsibleInventory.createGroup (toMap openshift-ansible-group)
        # Infra.AnsibleInventory.createGroup (toMap ospo-ansible-group)
    , hosts = Infra.AnsibleInventory.createHosts instances
    , instances
    }
