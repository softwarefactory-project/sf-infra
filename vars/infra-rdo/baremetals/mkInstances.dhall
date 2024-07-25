let rdo_domain = "rdoproject.org"

let sf_domain = "softwarefactory-project.io"

in  \(baremetal : Text) ->
    \(prefix : Text) ->
    \(subnet : Text) ->
      [ { name = "mirror.regionone.${prefix}-nodepool.${rdo_domain}"
        , ip = "${subnet}.10"
        , groups = [ "rhel", "private-afs-mirror" ]
        , proxy_jump = Some baremetal
        , ansible_user = "cloud-user"
        }
      , { name = "${prefix}-nodepool-launcher.${sf_domain}"
        , ip = "${subnet}.11"
        , groups =
          [ "rhel", "sf", "ibm-baremetal-nodepool", "ibm-instance", "promtail" ]
        , proxy_jump = Some baremetal
        , ansible_user = "cloud-user"
        }
      , { name = "${prefix}-ze.${sf_domain}"
        , ip = "${subnet}.12"
        , groups = [ "rhel", "sf", "ze", "ibm-instance", "promtail" ]
        , proxy_jump = Some baremetal
        , ansible_user = "cloud-user"
        }
      , { name = "${prefix}-zfgw.${sf_domain}"
        , ip = "${subnet}.13"
        , groups = [ "rhel", "sf", "ibm-instance", "promtail" ]
        , proxy_jump = Some baremetal
        , ansible_user = "cloud-user"
        }
      ]
