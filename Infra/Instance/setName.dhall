--| Set the name of an instance
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let setName
    : Instance.Type -> Text -> Instance.Type
    = \(instance : Instance.Type) -> \(name : Text) -> instance // { name }

let example0 =
        assert
      : setName Instance::{ name = "old" } "new" === Instance::{ name = "new" }

in  setName
