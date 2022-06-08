let Infra = ../../Infra/package.dhall

in  [ Infra.Instance::{
      , name = "baremetal02.rdoproject.org"
      , connection = Infra.Connection::{
        , ansible_user = "root"
        , ansible_host = Some "169.60.49.233"
        , ansible_python_interpreter = "auto"
        }
      }
    ]
