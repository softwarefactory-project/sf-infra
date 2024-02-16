{ boot_from_volume = "yes"
, flavor = Some "v1-standard-2"
, floating_ip = None Bool
, key_name = "sf-infra-key"
, name = ""
, network = "private-network"
, security_groups = [] : List Text
, state = (./State.dhall).present
, volume_size = None Natural
}
