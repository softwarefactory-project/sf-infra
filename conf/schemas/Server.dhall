{- The os_server task attribute -}
{ Type =
    { name : Text
    , boot_from_volume : Text
    , flavor : Optional Text
    , auto_ip : Optional Bool
    , floating_ips : Optional (List Text)
    , image : Text
    , key_name : Text
    , network : Text
    , security_groups : List Text
    , volume_size : Optional Natural
    , state : Optional Text
    }
, default =
    { flavor = let todo = "remove that default" in Some "v1-standard-2"
    , name = ""
    , auto_ip = Some False
    , floating_ips = None (List Text)
    , key_name = "sf-infra-key"
    , network = "private-network"
    , security_groups = [] : List Text
    , volume_size = None Natural
    , boot_from_volume = "yes"
    , state = None Text
    }
}
