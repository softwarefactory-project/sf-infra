let Infra = ../conf/package.dhall

let instances = ./instances.dhall

in  { instances = instances, servers = Infra.getServers instances }
