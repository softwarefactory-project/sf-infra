--| Return the name of an instance
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let getName
    : Instance.Type -> Text
    = \(instance : Instance.Type) -> instance.name

in  getName
