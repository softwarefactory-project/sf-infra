--| Extend the isntance name with an fqdn
let Instance = { Type = ./Type.dhall, default = ./default.dhall }

let setFqdn
    : Text -> Instance.Type -> Instance.Type
    = \(fqdn : Text) ->
      \(instance : Instance.Type) ->
        instance // { name = instance.name ++ "." ++ fqdn }

let example0 =
        assert
      :     setFqdn "softwarefactory-project.io" Instance::{ name = "www" }
        ===  Instance::{ name = "www.softwarefactory-project.io" }

in  setFqdn
