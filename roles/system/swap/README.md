Simple role to set up a swap device
===================================

This role will format a device as swap, add it to fstab, and enable swap
if required.

# Variables

The role will use the following variable, defined in the inventory:

* `swap_device`: Name of the device to be enabled as swap
