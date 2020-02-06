{- A dhall package that contains all the configuration -}
    ./conf/infra.dhall sha256:c7dc2a03280b48c889adf642b886fe67463d303ab5f12118c014a4d2171dd4c2
//  { SF =
        ./playbooks/vars/infra-sf.dhall sha256:26f906a64ae055a4b9e31132a4773edc87a3e91e64d229b9ac35b00917fb1140
    , RDO =
        ./playbooks/vars/infra-rdo.dhall sha256:d2530fef6ff393869a83b968836c46e4aac9dc6cf740bfe16ea8317ce6eccd2f
    }
