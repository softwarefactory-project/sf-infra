{-|
# AnsibleInventory

A package to convert `Instance`s to Ansible related configuration.
-}
{ createGroup = ./createGroup.dhall
, createHosts = ./createHosts.dhall
, Group = ./Group/package.dhall
, Host = ./Host/package.dhall
}
