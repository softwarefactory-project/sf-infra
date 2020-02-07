{- A dhall package that contains all the configuration -}
    ./conf/infra.dhall sha256:1012725f2bd58cb7067c36cf9c9f1816dded45810c5415ba51a798c7462b6b8b
//  { SF =
        ./playbooks/vars/infra-sf.dhall sha256:1a74ab85ebdd94f931612461380e5b677fa0d3a96fccd2d3efc6845b73fc7aa3
    , RDO =
        ./playbooks/vars/infra-rdo.dhall sha256:bca74426d056552a4364992a84c13d93fb4bcee6ea2842fc70af77dbe2c86fa7
    }
