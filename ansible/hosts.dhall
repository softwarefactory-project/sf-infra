let Infra = ../Infra/package.dhall

let instances = ../vars/instances.dhall

let {- special case for openshift-ansible that requires a group of groups -}
    openshift-ansible-group =
      { name = Infra.Group.Type.OSEv3
      , children =
        [ Infra.Group.Type.etcd
        , Infra.Group.Type.masters
        , Infra.Group.Type.nodes
        , Infra.Group.Type.installer
        ]
      }

in  { all =
      { hosts = Infra.AnsibleInventory.createHosts instances
      , children =
          Infra.AnsibleInventory.createGroup
            instances
            [ openshift-ansible-group ]
      }
    }
