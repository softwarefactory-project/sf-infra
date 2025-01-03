{- Global configuration defaults values -}
let Infra = ../Infra/package.dhall

let {- The official flavor list that is running on the Infra Aggregate
    This can be used for sf, rdo (not tripleo), dci and fedora jobs
    -} InfraAggregateFlavor =
      { `1vcpu_1gb` = "1vcpu_1gb"
      , `1vcpu_2gb` = "1vcpu_2gb"
      , `1vcpu_4gb` = "v1-standard-1"
      , `2vcpus_8gb` = "v1-standard-2"
      , `4vcpus_16gb` = "v1-standard-4"
      , `4vcpus_8gb` = "4vcpus_8gb"
      , `6vcpus_12gb` = "6vcpus_12gb"
      , `8vcpu_16GB` = "8vcpu_16GB"
      , `8vcpus_32gb` = "v1-standard-8"
      , `8vcpus_8gb` = "nodepool-infra"
      }

let {- The official flavor list that is running on the CI Aggregate
    This is reserved for tripleo jobs
    -} CIAggregateFlavor =
      { `8vcpus_8gb` = "nodepool"
      , `1vcpus_2gb` = "ci.m1.small"
      , `2vcpus_4gb` = "ci.m1.medium"
      , `4vcpus_8gb` = "ci.m1.large"
      }

let web-rules = [ Infra.Rule::{ port = +80 }, Infra.Rule::{ port = +443 } ]

let afs-rules =
      [ Infra.Rule::{ port = +8080 }
      , Infra.Rule::{ port = +8081 }
      , Infra.Rule::{ port = +8082 }
      , Infra.Rule::{ port = +8083 }
      , Infra.Rule::{ port = +8084 }
      , Infra.Rule::{ port = +4443 }
      , Infra.Rule::{ port = +4444 }
      , Infra.Rule::{ port = +4445 }
      , Infra.Rule::{ port = +4446 }
      , Infra.Rule::{ port = +4447 }
      ]

in  { sfInfraKeypair = ./files/infra_key.pub as Text
    , afs-rules
    , web-rules
    , SecurityGroups =
      [ { name = "common"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          ]
        }
      , { name = "web", rules = web-rules }
      , { name = "managesf"
        , rules =
          [ Infra.Rule::{ port = +1883 }
          , Infra.Rule::{ port = +1884 }
          , Infra.Rule::{ port = +29418 }
          , Infra.Rule::{ port = +64738 }
          , Infra.Rule::{ port = +64738, protocol = Some "udp" }
          ]
        }
      , { name = "internal"
        , rules =
          [ Infra.Rule::{
            , port = +1
            , port_range_min = Some +1
            , port_range_max = Some +65535
            , protocol = Some "tcp"
            , remote_ip_prefix = Some "192.168.0.0/16"
            }
          , Infra.Rule::{
            , port = +1
            , port_range_min = Some +1
            , port_range_max = Some +65535
            , protocol = Some "udp"
            , remote_ip_prefix = Some "192.168.0.0/16"
            }
          ]
        }
      ]
    , dns-servernames = [ "1.1.1.1", "8.8.8.8" ]
    , external-network = "public"
    , Flavors = InfraAggregateFlavor
    , TripleOFlavors = CIAggregateFlavor
    , OS =
        let Py2 = { ansible_python_interpreter = "python2" }

        let Py3 = { ansible_python_interpreter = "python3" }

        let PyAuto = { ansible_python_interpreter = "auto" }

        in  { CentOS =
                let CentOS = { ansible_user = Some "centos" }

                in  { `7.0` =
                      { connection = Infra.Connection::(CentOS // Py2)
                      , image = Infra.Image::{
                        , name = "centos-7-1907"
                        , url =
                            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
                        , checksum =
                            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
                        }
                      }
                    , `8.0` =
                      { connection = Infra.Connection::(     CentOS
                                                         //  PyAuto
                                                         //  { ansible_user =
                                                                 let comment =
                                                                       "For some reason, this image uses a cloud-user"

                                                                 in  Some
                                                                       "cloud-user"
                                                             }
                                                       )
                      , image = Infra.Image::{
                        , name = "centos-8.0-1905"
                        , url =
                            "https://jpena.fedorapeople.org/CentOS-8-GenericCloud-8.0.1905-22.x86_64.qcow2"
                        , checksum =
                            "c86c119665866695a700a4ab523c774c64ed7e0dd9e6e89f5f032e0f03148a47"
                        }
                      }
                    , `8.1` =
                      { connection = Infra.Connection::(CentOS // PyAuto)
                      , image = Infra.Image::{
                        , name = "centos-8.1-1911"
                        , url =
                            "https://cloud.centos.org/centos/8/x86_64/images/CentOS-8-GenericCloud-8.1.1911-20200113.3.x86_64.qcow2"
                        , checksum =
                            "e2cf1081645b1089f574918fb808b32d247169ec4ec1a13bca9e14a74df6530e"
                        }
                      }
                    , `8.3` =
                      { connection = Infra.Connection::(CentOS // PyAuto)
                      , image = Infra.Image::{
                        , name = "centos-8.3-2011"
                        , url =
                            "https://cloud.centos.org/centos/8/x86_64/images/CentOS-8-GenericCloud-8.3.2011-20201204.2.x86_64.qcow2"
                        , checksum =
                            "7ec97062618dc0a7ebf211864abf63629da1f325578868579ee70c495bed3ba0"
                        }
                      }
                    , `8-stream` =
                      { connection = Infra.Connection::(     CentOS
                                                         //  { ansible_python_interpreter =
                                                                 "/usr/libexec/platform-python"
                                                             }
                                                       )
                      , image = Infra.Image::{
                        , name = "centos-8-stream"
                        , url =
                            "https://cloud.centos.org/centos/8-stream/x86_64/images/CentOS-Stream-GenericCloud-8-20201019.1.x86_64.qcow2"
                        , checksum =
                            "68e5d217f8777789cbe5c47cda8776f4acfefa3968b7cef751ba08f651d0cf5a"
                        }
                      }
                    , `9-stream` =
                      { connection = Infra.Connection::(     CentOS
                                                         //  PyAuto
                                                         //  { ansible_user = Some
                                                                 "cloud-user"
                                                             }
                                                       )
                      , image = Infra.Image::{
                        , name = "centos-9-stream"
                        , url =
                            "https://cloud.centos.org/centos/9-stream/x86_64/images/CentOS-Stream-GenericCloud-9-20230605.0.x86_64.qcow2"
                        , checksum =
                            "6193a49c545425f99d23f59ea792f79a7a6b2e3557fc2d3f6d42c3e5274061de"
                        }
                      }
                    }
            , Fedora =
                let Fedora = Py3 // { ansible_user = Some "fedora" }

                let url =
                      "https://download.fedoraproject.org/pub/fedora/linux/releases/"

                in  { `30` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-30-1.2"
                        , state = "absent"
                        , url =
                                url
                            ++  "30/Cloud/x86_64/images/Fedora-Cloud-Base-30-1.2.x86_64.qcow2"
                        , checksum =
                            "72b6ae7b4ed09a4dccd6e966e1b3ac69bd97da419de9760b410e837ba00b4e26"
                        }
                      }
                    , `31` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-31-1.9"
                        , state = "absent"
                        , url =
                                url
                            ++  "31/Cloud/x86_64/images/Fedora-Cloud-Base-31-1.9.x86_64.qcow2"
                        , checksum =
                            "e3c1b309d9203604922d6e255c2c5d098a309c2d46215d8fc026954f3c5c27a0"
                        }
                      }
                    , `32`.connection = Infra.Connection::Fedora
                    , `33` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-33-1.2"
                        , state = "absent"
                        , url =
                                url
                            ++  "33/Cloud/x86_64/images/Fedora-Cloud-Base-33-1.2.x86_64.qcow2"
                        , checksum =
                            "7a2b3cc3bb3a92ce927e685d33efe8efd75577efbe207b267cb66f68afae7ce9"
                        }
                      }
                    , `35` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , state = "absent"
                        , name = "fedora-35-1.2"
                        , url =
                                url
                            ++  "35/Cloud/x86_64/images/Fedora-Cloud-Base-35-1.2.x86_64.qcow2"
                        , checksum =
                            "fe84502779b3477284a8d4c86731f642ca10dd3984d2b5eccdf82630a9ca2de6"
                        }
                      }
                    , `36` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-36-1.5"
                        , url =
                                url
                            ++  "36/Cloud/x86_64/images/Fedora-Cloud-Base-36-1.5.x86_64.qcow2"
                        , checksum =
                            "ca9e514cc2f4a7a0188e7c68af60eb4e573d2e6850cc65b464697223f46b4605"
                        }
                      }
                    , `37` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-37-1.7"
                        , url =
                                url
                            ++  "37/Cloud/x86_64/images/Fedora-Cloud-Base-37-1.7.x86_64.qcow2"
                        , checksum =
                            "b5b9bec91eee65489a5745f6ee620573b23337cbb1eb4501ce200b157a01f3a0"
                        }
                      }
                    , `38` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-38-1.6"
                        , url =
                                url
                            ++  "38/Cloud/x86_64/images/Fedora-Cloud-Base-38-1.6.x86_64.qcow2"
                        , checksum =
                            "d334670401ff3d5b4129fcc662cf64f5a6e568228af59076cc449a4945318482"
                        }
                      }
                    , `39` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-39-1.5"
                        , url =
                                url
                            ++  "39/Cloud/x86_64/images/Fedora-Cloud-Base-39-1.5.x86_64.qcow2"
                        , checksum =
                            "ab5be5058c5c839528a7d6373934e0ce5ad6c8f80bd71ed3390032027da52f37"
                        }
                      }
                    }
            , RHEL =
                let RHEL = { ansible_user = Some "cloud-user" }

                in  { `9.3` =
                      { -- image from https://access.redhat.com/downloads/content/479/ver=/rhel---9/9.3/x86_64/product-software
                        connection = Infra.Connection::(RHEL // PyAuto)
                      , image = Infra.Image::{
                        , name = "rhel-9.3-x86_64-kvm"
                        , url = "https://redhat.com"
                        , checksum =
                            "b1dd527a92721fda5469f511b5d6b4c97cfa18bbf9b002b0fcec08b4e067d28c"
                        }
                      }
                    , `9.4` =
                      { -- image from https://access.redhat.com/downloads/content/479/ver=/rhel---9/9.4/x86_64/product-software
                        connection = Infra.Connection::(RHEL // PyAuto)
                      , image = Infra.Image::{
                        , name = "rhel-9.4-x86_64-kvm"
                        , url = "https://redhat.com"
                        , checksum =
                            "d362e72518a2d7415d850b8177c814f6fd87f42ca1640bda17e98855e1d6baad"
                        }
                      }
                    }
            }
    }
