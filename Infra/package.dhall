{-|
# The dhall-infra packages

> Get started with dhall by reading [tutorial](https://docs.dhall-lang.org/tutorials/Getting-started_Generate-JSON-or-YAML.html)

This is a collection of dhall packages to manage an infrastructure with Ansible like sf-infra.

Most notably:

* The `Server` package define an OpenStack server
* The `Instance` package define a `Server` along with `Security-Group`, `Volumes`, `Connection`..

> Note that this dhall-infra package does not have to be part of sf-infra and it will
> be extracted once the API is more stable.


## Directory structure

Each directory contains a `package.dhall` that indexes all the `*.dhall` files and
the `*/package.dhall` for the subdirectories.

For example to access an attribute, this expression:

```dhall
let Infra = ./package.dhall
in Infra.Instance.Type
```

is equivalent to `./Instance/Type.dhall`.

> The `package.dhall` can be generated using the shake command, see the `Shakefile.hs` file


## Schemas

When a package contains a `Type` and `default` attribute, they can be used as a completable record:

```dhall
let Infra = ./package.dhall
in Infra.Rule::{ port = +29418 }
```

(which is equivalent to `(./Rule/package.dhall)::{ port = +29418 }`

> For more information, see [Language Tour documentation](https://docs.dhall-lang.org/tutorials/Language-Tour.html#record-completion)


## Function

Packages also contain functions related to their associated Type.
See for example: `Network/create.dhall`.

The documentation is also available in HTML at https://docs.softwarefactory-project.io/dhall-infra/


## Group

The Group package was incorrectly modeled as an Union of possible group
and a future change will remove it from the Infra package.
-}
{ AnsibleInventory = ./AnsibleInventory/package.dhall
, Backup = ./Backup/package.dhall
, Connection = ./Connection/package.dhall
, Firewall = ./Firewall/package.dhall
, Image = ./Image/package.dhall
, Instance = ./Instance/package.dhall
, Keypair = ./Keypair/package.dhall
, Network = ./Network/package.dhall
, OpenShift = ./OpenShift/package.dhall
, Router = ./Router/package.dhall
, Rule = ./Rule/package.dhall
, SecurityGroup = ./SecurityGroup/package.dhall
, Server = ./Server/package.dhall
, Subnet = ./Subnet/package.dhall
, Tenant = ./Tenant/package.dhall
, Volume = ./Volume/package.dhall
}
