{ Type =
    { ansible_user : Text
    , ansible_host : Optional Text
    , ansible_python_interpreter : Text
    , ansible_port : Natural
    , ansible_become : Optional Bool
    , proxy_command : Optional Text
    }
, default =
    { ansible_port = 22
    , ansible_host = None Text
    , ansible_python_interpreter = "auto"
    , ansible_become = None Bool
    , proxy_command = None Text
    }
}
