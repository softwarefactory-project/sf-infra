{- A package that mix the conf and vars directory.

This enables usage such as:

# Get the list of all the managed hostnames:
$ dhall <<< 'let Infra = ./package.dhall in Infra.Server.map Text Infra.Server.getName Infra.servers'
[ "bridge.softwarefactory-project.io"
, "prometheus.monitoring.softwarefactory-project.io"
...
]
-}
./conf/package.dhall // ./vars/package.dhall
