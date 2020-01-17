Simple role to set up a volume device
===================================

This role will format a device, add it to fstab, and mount it

# Variables

The role will use the following variables, defined in the inventory:

* `device`: Name of the device
* `mountpoint`: The directory used to mount the device
* `filesystem`: The fs type
* `owner`: Directory owner
* `group`: Directory group
