let baremetal = { name = "baremetal03.rdoproject.org", ip = "169.60.49.226" }

let subnet = "192.168.25"

let instances = ./mkInstances.dhall baremetal.name "ibm-bm3" subnet

in  { baremetal, instances, subnet }
