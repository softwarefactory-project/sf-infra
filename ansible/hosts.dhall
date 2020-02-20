let Infra = ../conf/package.dhall

let instances = ../vars/instances.dhall

in  { all =
        { hosts = Infra.mkHost instances, children = Infra.mkGroup instances }
    }
