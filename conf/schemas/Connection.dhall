{ Type =
    { ansible_user : Text
    , ansible_python_interpreter : Text
    , ansible_port : Natural
    }
, default = { ansible_port = 22, ansible_python_interpreter = "auto" }
}
