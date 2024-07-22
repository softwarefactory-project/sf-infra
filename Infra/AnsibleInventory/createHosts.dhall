--| A function to create the inventory hosts list
let Prelude = ../Prelude.dhall

let AnsibleInventory = { Host = ./Host/package.dhall }

let Instance = ../Instance/package.dhall

let createHosts
    : List Instance.Type -> List AnsibleInventory.Host.Type
    = Instance.map
        AnsibleInventory.Host.Type
        ( \(instance : Instance.Type) ->
            { mapKey = instance.name
            , mapValue =
                  merge
                    { None = AnsibleInventory.Host.empty
                    , Some =
                        \(user : Text) ->
                          toMap { ansible_user = Prelude.JSON.string user }
                    }
                    instance.connection.ansible_user
                # toMap
                    { ansible_python_interpreter =
                        Prelude.JSON.string
                          instance.connection.ansible_python_interpreter
                    , ansible_port =
                        Prelude.JSON.natural instance.connection.ansible_port
                    }
                # merge
                    { None = AnsibleInventory.Host.empty
                    , Some =
                        \(some : Bool) ->
                          toMap { ansible_become = Prelude.JSON.bool some }
                    }
                    instance.connection.ansible_become
                # merge
                    { None = AnsibleInventory.Host.empty
                    , Some =
                        \(method : Text) ->
                          toMap
                            { ansible_become_method = Prelude.JSON.string method
                            }
                    }
                    instance.connection.ansible_become_method
                # merge
                    { None = AnsibleInventory.Host.empty
                    , Some =
                        \(command : Text) ->
                          toMap
                            { ansible_ssh_common_args =
                                Prelude.JSON.string
                                  "-o ProxyCommand=\"${command}\""
                            }
                    }
                    instance.connection.proxy_command
                # merge
                    { None = AnsibleInventory.Host.empty
                    , Some =
                        \(ip-addr : Text) ->
                          toMap { ansible_host = Prelude.JSON.string ip-addr }
                    }
                    instance.connection.ansible_host
            }
        )

in  createHosts
