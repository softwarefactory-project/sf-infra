--| A function to create the inventory group of group list
let Prelude = ../Prelude.dhall

let AnsibleInventory = { Group = ./Group/package.dhall }

let Instance = ../Instance/package.dhall

let createGroup
    : Prelude.Map.Type Text (List Text) ->
        Prelude.Map.Type Text AnsibleInventory.Group.Type
    = \(group-of-group : Prelude.Map.Type Text (List Text)) ->
        let createGroupValue =
              Prelude.List.map
                Text
                (Prelude.Map.Entry Text {})
                (\(name : Text) -> { mapKey = name, mapValue = {=} })

        let createGroupEntry
            : Prelude.Map.Entry Text (List Text) ->
                Prelude.Map.Entry Text AnsibleInventory.Group.Type
            = \(entry : Prelude.Map.Entry Text (List Text)) ->
                { mapKey = entry.mapKey
                , mapValue = AnsibleInventory.Group::{
                  , children = Some (createGroupValue entry.mapValue)
                  }
                }

        in  Prelude.List.map
              (Prelude.Map.Entry Text (List Text))
              (Prelude.Map.Entry Text AnsibleInventory.Group.Type)
              createGroupEntry
              group-of-group

in  createGroup
