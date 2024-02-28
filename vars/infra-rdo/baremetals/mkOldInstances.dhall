let rdo_domain = "rdoproject.org"

let sf_domain = "softwarefactory-project.io"

in  \(baremetal : Text) ->
    \(prefix : Text) ->
    \(subnet : Text) ->
      [ { name = "mirror.regionone.${prefix}-nodepool.${rdo_domain}"
        , ip = "${subnet}.10"
        , groups = [ "private-afs-mirror" ]
        , proxy_jump = Some baremetal
        , ansible_user = "centos"
        }
      , { name = "${prefix}-nodepool-launcher.${sf_domain}"
        , ip = "${subnet}.11"
        , groups = [ "sf", "ibm-baremetal-nodepool", "ibm-instance" ]
        , proxy_jump = Some baremetal
        , ansible_user = "centos"
        }
      , { name = "${prefix}-ze.${sf_domain}"
        , ip = "${subnet}.12"
        , groups = [ "sf", "ze", "ibm-instance" ]
        , proxy_jump = Some baremetal
        , ansible_user = "centos"
        }
      , { name = "${prefix}-zfgw.${sf_domain}"
        , ip = "${subnet}.13"
        , groups = [ "sf", "ibm-instance" ]
        , proxy_jump = Some baremetal
        , ansible_user = "centos"
        }
      ]
