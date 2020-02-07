{- A dhall package that contains all the configuration -}
    ./conf/infra.dhall sha256:1012725f2bd58cb7067c36cf9c9f1816dded45810c5415ba51a798c7462b6b8b
//  { SF =
        ./playbooks/vars/infra-sf.dhall sha256:8b89d1ea4b8f54eecd80d7c750c5971c7142c8a3ca39e05af77f37fff3a8c080
    , RDO =
        ./playbooks/vars/infra-rdo.dhall sha256:bca74426d056552a4364992a84c13d93fb4bcee6ea2842fc70af77dbe2c86fa7
    }
