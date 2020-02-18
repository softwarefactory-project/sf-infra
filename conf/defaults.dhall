{- Global configuration defaults values -}
{ sfInfraKeypair = ./files/infra_key.pub as Text
, SecurityGroups =
  [ { name = "common"
    , rules =
      [ (./schemas/Rule.dhall)::{ port = +22 }
      , (./schemas/Rule.dhall)::{ port = -1, protocol = Some "icmp" }
      ]
    }
  , { name = "web"
    , rules =
      [ (./schemas/Rule.dhall)::{ port = +80 }
      , (./schemas/Rule.dhall)::{ port = +443 }
      ]
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
, OS =
    let Py2 = { ansible_python_interpreter = "python2" }

    let Py3 = { ansible_python_interpreter = "python3" }

    in  { CentOS =
            let CentOS = { ansible_user = "centos" }

            in  { `7.0` = CentOS // Py2 // { image = "centos-7-1907" }
                , `8.0` = CentOS // Py3 // { image = "centos-8.0-1905" }
                , `8.1` = CentOS // Py3 // { image = "centos-8.1-1911" }
                }
        , Fedora =
            let Fedora = Py3 // { ansible_user = "fedora" }

            in  { `30` = Fedora // { image = "fedora-30-1.2" }
                , `31` = Fedora // { image = "fedora-31-1.9" }
                }
        }
}
