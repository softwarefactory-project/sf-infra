let baremetal = { name = "baremetal04.rdoproject.org", ip = "169.60.49.233" }

let instances = ./mkInstances.dhall baremetal.name "ibm-bm4" "192.168.26"

in  { baremetal, instances }
