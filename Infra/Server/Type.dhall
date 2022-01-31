-- | Set auto_ip to True to get a floating ip
{ auto_ip : Optional Bool
, boot_from_volume : Text
, flavor : Optional Text
, floating_ips : Optional (List Text)
, floating_ip_pools : Optional (List Text)
, image : Text
, key_name : Text
, name : Text
, network : Text
, security_groups : List Text
, state : Optional Text
, volume_size : Optional Natural
}
