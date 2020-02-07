{- A dhall package that contains all the configuration -}
    ./conf/infra.dhall sha256:1012725f2bd58cb7067c36cf9c9f1816dded45810c5415ba51a798c7462b6b8b
//  { SF =
        ./playbooks/vars/infra-sf.dhall sha256:1a74ab85ebdd94f931612461380e5b677fa0d3a96fccd2d3efc6845b73fc7aa3
    , RDO =
        ./playbooks/vars/infra-rdo.dhall sha256:d2530fef6ff393869a83b968836c46e4aac9dc6cf740bfe16ea8317ce6eccd2f
    }
