{- A dhall package that contains all the configuration -}
    ./conf/infra.dhall sha256:60d16dec7dd8638bd014bc0a96f177e6ed78399cbd6e62ca86eec03f30a4d24d
//  { SF =
        ./playbooks/vars/infra-sf.dhall sha256:c85ef6401cba23937b9ea001248b2a502fa84c17f200c77e31743f9fb5ff258d
    , RDO =
        ./playbooks/vars/infra-rdo.dhall sha256:d2530fef6ff393869a83b968836c46e4aac9dc6cf740bfe16ea8317ce6eccd2f
    }
