let Infra = ../conf/package.dhall

let instances = ../vars/instances.dhall

let {- special case for openshift-ansible that requires a group of groups -}
    openshift-ansible-group =
      { name = Infra.Group.OSEv3
      , children =
        [ Infra.Group.etcd
        , Infra.Group.masters
        , Infra.Group.nodes
        , Infra.Group.installer
        ]
      }

in  { all =
        { hosts = Infra.mkHost instances
        , children = Infra.mkGroup instances [ openshift-ansible-group ]
        }
    }
