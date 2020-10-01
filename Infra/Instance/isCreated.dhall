--| Check if an instance has been created
let Instance = { Type = ./Type.dhall }

let isCreated
    : Instance.Type -> Bool
    = \(instance : Instance.Type) -> instance.skip_os_server_task == False

in  isCreated
