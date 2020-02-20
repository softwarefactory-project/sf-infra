{- Global configuration defaults values -}
let Infra = ../conf/package.dhall

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
      ]
    , external-network = "public"
    , Flavors =
        { `1vcpu_1gb` = "1vcpu_1gb"
        , `1vcpu_2gb` = "1vcpu_2gb"
        , `1vcpu_4gb` = "v1-standard-1"
        , `2vcpus_8gb` = "v1-standard-2"
        , `4vcpus_16gb` = "v1-standard-4"
        , `4vcpus_8gb` = "4vcpus_8gb"
        , `6vcpus_12gb` = "6vcpus_12gb"
        , `8vcpus_32gb` = "v1-standard-8"
        }
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
                        , image = "centos-7-1907"
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
                        , image = "centos-8.0-1905"
                        }
                    , `8.1` =
                        { connection = Infra.Connection::(CentOS // PyAuto)
                        , image = "centos-8.1-1911"
                        }
                    }
            , Fedora =
                let Fedora = Py3 // { ansible_user = "fedora" }

                in  { `30` =
                        { connection = Infra.Connection::Fedora
                        , image = "fedora-30-1.2"
                        }
                    , `31` =
                        { connection = Infra.Connection::Fedora
                        , image = "fedora-31-1.9"
                        }
                    }
            }
    }
