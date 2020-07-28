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

in  { sfInfraKeypair = ./files/infra_key.pub as Text
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
    }
