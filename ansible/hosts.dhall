let Infra = ../package.dhall

let servers = Infra.SF.servers # Infra.RDO.servers

in  { all = { children = Infra.mkGroup servers } }
