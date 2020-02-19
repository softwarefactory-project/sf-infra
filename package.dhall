{- A dhall package that contains all the configuration -}
let SF = ./playbooks/vars/infra-sf.dhall

let RDO = ./playbooks/vars/infra-rdo.dhall

in      ./conf/infra.dhall
    //  { SF = SF, RDO = RDO, servers = SF.servers # RDO.servers }
