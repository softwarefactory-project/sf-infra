{ Type =
    { name : Text
    , boot_from_volume : Text
    , flavor : Text
    , auto_ip : Optional Bool
    , floating_ips : Optional (List Text)
    , image : Text
    , key_name : Text
    , network : Text
    , security_groups : List Text
    , volume_size : Optional Natural
    , state : Optional Text
    , groups : Optional (List ../types/Group.dhall)
    }
, default =
    { flavor = (../defaults.dhall).Flavors.`2vcpus_8gb`
    , auto_ip = Some False
    , floating_ips = None (List Text)
    , image = "centos-7-1907"
    , key_name = "sf-infra-key"
    , network = "private-network"
    , security_groups = [] : List Text
    , volume_size = None Natural
    , boot_from_volume = "yes"
    , state = None Text
    , groups = None (List ../types/Group.dhall)
    }
}
