--| Return the server of an instance
\(instance : ./Type.dhall) -> instance.server // { name = instance.name }
