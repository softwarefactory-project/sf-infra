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

                in  { `8.1` =
                        {- used by dashboards.rdoproject.org -}
                        { connection = Infra.Connection::(CentOS // PyAuto)
                        , image = Infra.Image::{
                          , name = "centos-8.1-1911"
                          , url =
                              "https://cloud.centos.org/centos/8/x86_64/images/CentOS-8-GenericCloud-8.1.1911-20200113.3.x86_64.qcow2"
                          , checksum =
                              "e2cf1081645b1089f574918fb808b32d247169ec4ec1a13bca9e14a74df6530e"
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

                in  { `43` =
                      { connection = Infra.Connection::Fedora
                      , image = Infra.Image::{
                        , name = "fedora-43-1.6"
                        , state = "absent"
                        , url =
                                url
                            ++  "43/Cloud/x86_64/images/Fedora-Cloud-Base-Generic-43-1.6.x86_64.qcow2"
                        , checksum =
                            "846574c8a97cd2d8dc1f231062d73107cc85cbbbda56335e264a46e3a6c8ab2f"
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
                        , state = "absent"
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
                    , `10.1` =
                      { -- image from https://access.redhat.com/downloads/content/479/ver=/rhel---10/10.1/x86_64/product-software
                        connection = Infra.Connection::(RHEL // PyAuto)
                      , image = Infra.Image::{
                        , name = "rhel-10.1-x86_64-kvm"
                        , url = "https://redhat.com"
                        , checksum =
                            "dc74ad1a9ccd3c62a02cc29f7f4715e47fa0fdaf08a8080dd20906f885f29bae"
                        }
                      }
                    }
            }
    }
