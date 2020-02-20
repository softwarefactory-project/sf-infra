let Infra = ../../conf/package.dhall

in  { images =
      [ Infra.Image::{
        , name = "fedora-31-1.9"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/31/Cloud/x86_64/images/Fedora-Cloud-Base-31-1.9.x86_64.qcow2"
        , checksum =
            "e3c1b309d9203604922d6e255c2c5d098a309c2d46215d8fc026954f3c5c27a0"
        }
      , Infra.Image::{
        , name = "fedora-30-1.2"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/30/Cloud/x86_64/images/Fedora-Cloud-Base-30-1.2.x86_64.qcow2"
        , checksum =
            "72b6ae7b4ed09a4dccd6e966e1b3ac69bd97da419de9760b410e837ba00b4e26"
        }
      , Infra.Image::{
        , name = "centos-7-1907"
        , url =
            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
        , checksum =
            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
        }
      ]
    }
