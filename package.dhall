{- A dhall package that contains all the configuration -}
    ./conf/common.dhall sha256:11627d1f821ebdbc9a52cf0fd6cf11fe43cd2170cd268cda58687e6fb21f7130
//  { SF =
        ./playbooks/vars/infra-sf.dhall sha256:bc5f774f42fa1dcef6c8172e9c9a21ad00dd5d7bab577a67bccec975f3ae793a
    , RDO =
        ./playbooks/vars/infra-rdo.dhall sha256:e958f1912f2df8a3c9654d79e30c9a9197ea3a0bf9852b2dcf3ff4cefb5ae1d5
    }
