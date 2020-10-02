--| Return the list of created instances
let Instance = ../Instance/package.dhall

in  \(instances : List Instance.Type) ->
      Instance.getServers (Instance.filter Instance.isCreated instances)
