{- The os_security_group_rule task attribute -}
{ Type =
    { port : Integer
    , protocol : Optional Text
    , remote_ip_prefix : Optional Text
    , port_range_max : Optional Integer
    , port_range_min : Optional Integer
    , state : Optional Text
    }
, default =
    { protocol = None Text
    , remote_ip_prefix = None Text
    , port_range_max = None Integer
    , port_range_min = None Integer
    , state = None Text
    }
}
