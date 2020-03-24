{- Global configuration defaults values -}
let Infra = ../conf/package.dhall

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

in  { sfInfraKeypair = ./files/infra_key.pub as Text
    , SecurityGroups =
      [ { name = "common"
        , rules =
          [ Infra.Rule::{ port = +22 }
          , Infra.Rule::{ port = -1, protocol = Some "icmp" }
          ]
        }
      , { name = "web"
        , rules = [ Infra.Rule::{ port = +80 }, Infra.Rule::{ port = +443 } ]
        }
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
    , external-network = "public"
    , Flavors = InfraAggregateFlavor
    , TripleOFlavors = CIAggregateFlavor
    , ExternalServer =
        { skip_os_server_task = True
        , server = Infra.Server::{ image = "unknown" }
        }
    , OS =
        let Py2 = { ansible_python_interpreter = "python2" }

        let Py3 = { ansible_python_interpreter = "python3" }

        let PyAuto = { ansible_python_interpreter = "auto" }

        in  { CentOS =
                let CentOS = { ansible_user = "centos" }

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

                                                                   in  "cloud-user"
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
                    }
            , Fedora =
                let Fedora = Py3 // { ansible_user = "fedora" }

                let url =
                      "https://download.fedoraproject.org/pub/fedora/linux/releases/"

                in  { `30` =
                        { connection = Infra.Connection::Fedora
                        , image = Infra.Image::{
                          , name = "fedora-30-1.2"
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
                          , url =
                                  url
                              ++  "31/Cloud/x86_64/images/Fedora-Cloud-Base-31-1.9.x86_64.qcow2"
                          , checksum =
                              "e3c1b309d9203604922d6e255c2c5d098a309c2d46215d8fc026954f3c5c27a0"
                          }
                        }
                    }
            }
    }
