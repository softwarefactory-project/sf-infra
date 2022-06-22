let Infra = ../../Infra/package.dhall

in  [ Infra.Instance::{
      , name = "baremetal02.rdoproject.org"
      , connection = Infra.Connection::{
        , ansible_user = "root"
        , ansible_host = Some "169.60.49.233"
        , ansible_python_interpreter = "auto"
        }
      }
    , Infra.Instance::{
      , name = "mirror.regionone.ibm-bm2-nodepool.rdoproject.org"
      , groups = [ "afs-mirror" ]
      , node-exporter = False
      , connection = Infra.Connection::{
        , ansible_user = "centos"
        , ansible_host = Some "192.168.25.35"
        , ansible_python_interpreter = "python2"
        , proxy_jump = Some "baremetal02.rdoproject.org"
        }
      }
    , Infra.Instance::{
      , name = "ibm-nodepool-launcher.softwarefactory-project.io"
      , groups = [ "sf", "ibm-bm2-nodepool" ]
      , node-exporter = False
      , connection = Infra.Connection::{
        , ansible_user = "centos"
        , ansible_host = Some "192.168.25.195"
        , ansible_python_interpreter = "python2"
        , proxy_jump = Some "baremetal02.rdoproject.org"
        }
      }
    , Infra.Instance::{
      , name = "ibm-ze.softwarefactory-project.io"
      , groups = [ "sf", "ze" ]
      , node-exporter = False
      , connection = Infra.Connection::{
        , ansible_user = "centos"
        , ansible_host = Some "192.168.25.127"
        , ansible_python_interpreter = "python2"
        , proxy_jump = Some "baremetal02.rdoproject.org"
        }
      }
    ]
