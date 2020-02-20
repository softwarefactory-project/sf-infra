let Infra = ../../conf/package.dhall

in  { images =
      [ Infra.Image::{
        , name = "centos-7-1907"
        , url =
            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
        , checksum =
            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
        }
      , Infra.Image::{
        , name = "fedora-30-1.2"
        , url =
            "https://download.fedoraproject.org/pub/fedora/linux/releases/30/Cloud/x86_64/images/Fedora-Cloud-Base-30-1.2.x86_64.qcow2"
        , checksum =
            "72b6ae7b4ed09a4dccd6e966e1b3ac69bd97da419de9760b410e837ba00b4e26"
        }
      , Infra.Image::{
        , name = "centos-8.0-1905"
        , url =
            "https://jpena.fedorapeople.org/CentOS-8-GenericCloud-8.0.1905-22.x86_64.qcow2"
        , checksum =
            "c86c119665866695a700a4ab523c774c64ed7e0dd9e6e89f5f032e0f03148a47"
        }
      , Infra.Image::{
        , name = "centos-8.1-1911"
        , url =
            "https://cloud.centos.org/centos/8/x86_64/images/CentOS-8-GenericCloud-8.1.1911-20200113.3.x86_64.qcow2"
        , checksum =
            "e2cf1081645b1089f574918fb808b32d247169ec4ec1a13bca9e14a74df6530e"
        }
      ]
    }
