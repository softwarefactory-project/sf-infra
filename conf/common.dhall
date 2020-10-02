let Infra = ./schemas/package.dhall

in  { ExternalServer =
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
