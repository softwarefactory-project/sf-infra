-- | Set floating_ip to True to get a floating ip
{ boot_from_volume : Text
, flavor : Optional Text
, floating_ip : Optional Bool
, image : Text
, key_name : Text
, name : Text
, network : Text
, security_groups : List Text
, state : ./State.dhall
, volume_size : Optional Natural
}
