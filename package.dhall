{- A dhall package that contains all the configuration -}
    ./conf/infra.dhall sha256:60d16dec7dd8638bd014bc0a96f177e6ed78399cbd6e62ca86eec03f30a4d24d
//  { SF =
        ./playbooks/vars/infra-sf.dhall sha256:99b900e07bd0dc3aebc6bfc3ab2cabbe4b0605a78d4c6af2b86254d40170d938
    , RDO =
        ./playbooks/vars/infra-rdo.dhall sha256:d2530fef6ff393869a83b968836c46e4aac9dc6cf740bfe16ea8317ce6eccd2f
    }
