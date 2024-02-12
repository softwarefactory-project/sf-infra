{ auto_ip = Some False
, boot_from_volume = "yes"
, flavor = Some "v1-standard-2"
, floating_ips = None (List Text)
, key_name = "sf-infra-key"
, name = ""
, network = "private-network"
, security_groups = [] : List Text
, state = (./State.dhall).present
, volume_size = None Natural
}
