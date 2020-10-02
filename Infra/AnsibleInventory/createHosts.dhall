--| A function to create the inventory hosts list
let Prelude = ../Prelude.dhall

let Instance = ../Instance/package.dhall

let HostVarValue = < int : Natural | str : Text | bool : Bool >

let emptyVars = [] : List { mapKey : Text, mapValue : HostVarValue }

in  Instance.map
      { mapKey : Text
      , mapValue : List { mapKey : Text, mapValue : HostVarValue }
      }
      ( \(instance : Instance.Type) ->
          { mapKey = instance.name
          , mapValue =
                toMap
                  { ansible_user =
                      HostVarValue.str instance.connection.ansible_user
                  , ansible_python_interpreter =
                      HostVarValue.str
                        instance.connection.ansible_python_interpreter
                  , ansible_port =
                      HostVarValue.int instance.connection.ansible_port
                  }
              # merge
                  { None = emptyVars
                  , Some =
                      \(some : Bool) ->
                        toMap { ansible_become = HostVarValue.bool some }
                  }
                  instance.connection.ansible_become
              # merge
                  { None = emptyVars
                  , Some =
                      \(command : Text) ->
                        toMap
                          { ansible_ssh_common_args =
                              HostVarValue.str "-o ProxyCommand=\"${command}\""
                          }
                  }
                  instance.connection.proxy_command
              # merge
                  { None = emptyVars
                  , Some =
                      \(ip-addr : Text) ->
                        toMap { ansible_host = HostVarValue.str ip-addr }
                  }
                  instance.connection.ansible_host
          }
      )
