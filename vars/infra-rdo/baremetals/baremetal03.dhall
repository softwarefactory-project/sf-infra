let baremetal = { name = "baremetal03.rdoproject.org", ip = "169.60.49.226" }

let instances = ./mkInstances.dhall baremetal.name "ibm-bm3" "192.168.25"

in  { baremetal, instances }
