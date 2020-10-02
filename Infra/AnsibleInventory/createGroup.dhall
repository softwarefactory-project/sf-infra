--| A function to create the inventory group list
let Prelude = ../Prelude.dhall

let Instance = ../Instance/package.dhall

let GroupOfGroup =
      { name : ../Group/Type.dhall, children : List ../Group/Type.dhall }

let Group = ../Group/Type.dhall

let Groups = ../Group/Groups.dhall

in  \(instances : List Instance.Type) ->
    \(group-of-groups : List GroupOfGroup) ->
      let ChildrenValue = { mapKey : Group, mapValue : {} }

      let HostsValue = { mapKey : Text, mapValue : {} }

      let ChildrensValue =
            { Type =
                { hosts : Optional (List HostsValue)
                , children : Optional (List ChildrenValue)
                }
            , default =
              { hosts = None (List HostsValue)
              , children = None (List ChildrenValue)
              }
            }

      let {- Create a group value for a given groupType
          -} mkGroupOfHost =
            let {- Convert list of instance to their name dict
                -} instanceName =
                  Instance.map
                    HostsValue
                    ( \(instance : Instance.Type) ->
                        { mapKey = instance.name, mapValue = {=} }
                    )

            let {- Check if a instance is part of a group
                -} filterInstance =
                  \(group : Groups.Type) ->
                  \(instance : Instance.Type) ->
                    Prelude.List.fold
                      Group
                      instance.groups
                      Bool
                      ( \(group : Group) ->
                        \(acc : Bool) ->
                          acc || group@1.test group
                      )
                      False

            in  \(group : Groups.Type) ->
                  { mapKey = group.value
                  , mapValue = ChildrensValue::{
                    , hosts = Some
                        ( instanceName
                            ( Prelude.List.filter
                                Instance.Type
                                (filterInstance group)
                                instances
                            )
                        )
                    }
                  }

      let {- Create a group of group value in inventory
          -} mkGroupOfGroup =
            \(gog : GroupOfGroup) ->
              let {- Convert list of group to their name dict
                  -} groupName =
                    Prelude.List.map
                      Group
                      ChildrenValue
                      (\(group : Group) -> { mapKey = group, mapValue = {=} })

              in  { mapKey = gog.name
                  , mapValue = ChildrensValue::{
                    , children = Some (groupName gog.children)
                    }
                  }

      let ChildrenDict = { mapKey : Group, mapValue : ChildrensValue.Type }

      in    Prelude.List.map
              Groups.Type
              ChildrenDict
              mkGroupOfHost
              Groups.groups
          # Prelude.List.map
              GroupOfGroup
              ChildrenDict
              mkGroupOfGroup
              group-of-groups
