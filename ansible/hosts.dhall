let Infra = ../Infra/package.dhall

let {- special case for openshift-ansible that requires a group of groups -}
    openshift-ansible-group =
      { OSEv3 = [ "etcd", "masters", "nodes", "installer" ] }

in  { groups =
        Infra.AnsibleInventory.createGroup (toMap openshift-ansible-group)
    , hosts = Infra.AnsibleInventory.createHosts ../vars/instances.dhall
    , instances = ../vars/instances.dhall
    }
