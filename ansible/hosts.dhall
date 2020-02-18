let Infra = ../package.dhall

let servers = Infra.SF.servers # Infra.RDO.servers

in  { all = { hosts = Infra.mkHost servers, children = Infra.mkGroup servers } }
