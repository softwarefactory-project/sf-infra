let Infra = ../../conf/package.dhall

let {- Note (dmsimard): The name "CentOS-7-x86_64-GenericCloud" is expected/hardcoded in different TripleO places
    -} images =
      [ Infra.Image::{
        , name = "CentOS-7-x86_64-GenericCloud"
        , url =
            "https://cloud.centos.org/centos/7/images/CentOS-7-x86_64-GenericCloud-1907.qcow2"
        , checksum =
            "520d01c2f2e1ed24cb2f15a4325aa30773930a2961b5484a68cf11b4a415c512"
        }
      , Infra.Image::{
        , name = "ipxe-boot"
        , url =
            "https://opendev.org/openstack/openstack-virtual-baremetal/raw/commit/b5b7791180b92644b680d738b383d1254f5a1e35/ipxe/ipxe-boot.qcow2"
        , checksum =
            let {- this is a comment -} todo =
                  ''
                  # TODO: bmc-template / bmc-base
                  # https://openstack-virtual-baremetal.readthedocs.io/en/latest/host-cloud/prepare.html
                  # https://opendev.org/openstack/tripleo-ci/src/branch/master/scripts/deploy-server.sh
                  ''

            in  "5a6f067ec4a9da1f13ab00a8031570cd877edc250bd08a412c376e59aa646536"
        }
      ]

in  { images }
