let baremetal = { name = "baremetal04.rdoproject.org", ip = "169.60.49.233" }

let subnet = "192.168.26"

let instances = ./mkOldInstances.dhall baremetal.name "ibm-bm4" subnet

in  { baremetal, instances, subnet }
