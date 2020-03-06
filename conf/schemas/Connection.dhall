{ Type =
    { ansible_user : Text
    , ansible_python_interpreter : Text
    , ansible_port : Natural
    , ansible_become : Optional Bool
    }
, default =
    { ansible_port = 22
    , ansible_python_interpreter = "auto"
    , ansible_become = None Bool
    }
}
