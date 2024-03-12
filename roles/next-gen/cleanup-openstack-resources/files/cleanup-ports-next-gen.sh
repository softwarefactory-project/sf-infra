#!/bin/bash

OS_CLOUD=${OS_CLOUD:-$1}
CRC_VENV_DIR=${CRC_VENV_DIR:-$2}
DAY_AGO=$(date -d "$(date '+%Y-%m-%dT%H:%M:%S' -d '1 day ago')" +%s)
ZUUL_PREFFIX=${ZUUL_PREFFIX:-'zuul-ci'}

if [ -z "${OS_CLOUD}" ]; then
    echo "You need to specify OS_CLOUD resource name. Can not continue"
    exit 1
fi

if [ -d "$CRC_VENV_DIR" ]; then
    echo "Using venv: $CRC_VENV_DIR"
    source $CRC_VENV_DIR/bin/activate
fi

# Remove port associated with router + remove router
for router in $(openstack router list | grep "$ZUUL_PREFFIX-subnet" | awk '{print $2}'); do
    ROUTER_DATE=$(date -d "$(openstack router show "$router" -c created_at -f value  )" +%s);
    echo "Router: $router date is: $(date -d@$ROUTER_DATE)"
    if [ "$DAY_AGO" -gt "$ROUTER_DATE" ]; then
        echo "Doing router: $router";
        router_port=$(openstack port list --router "$router" -f value -c id)
        echo "Removing router port: $router_port from router: $router";
        openstack router remove port "$router" "$router_port";
        echo "Removing router: $router"
        openstack router delete "$router";
    fi
done

# Unset trunk ports networks
for trunk_network in $(openstack network trunk list | grep -E "$ZUUL_PREFFIX-trunk|default-trunk" | awk '{print $2}'); do
    NETWORK_DATE=$(date -d "$(openstack network trunk show "$trunk_network" -c created_at -f value)" +%s);
    echo "Trunk network $trunk_network date is: $(date -d@$NETWORK_DATE)"
    if [ "$DAY_AGO" -gt "$NETWORK_DATE" ]; then
        echo "Unset trunk ports for network: $trunk_network"
        for trunk_port in $(openstack network trunk show "$trunk_network" -c sub_ports -f value | grep -o "'port_id': '[^']*'" | awk -F"'" '{print $4}'); do
            openstack network trunk unset "$trunk_network" --subport "$trunk_port"
        done
        # NOTE: Here we can not remove the network, due it can have assigned ports!
        # We will do that after removing ports in subnets.
    fi
done

# Remove ports connected to network, then remove subnets for network and later delete network
for network in $(openstack network list | grep -E "$ZUUL_PREFFIX-net|default-cifmw|internal-api-cifmw|storage-cifmw|tenant-cifmw" | awk '{print $2}'); do
    NETWORK_DATE=$(date -d "$(openstack network show "$network" -c created_at -f value)" +%s);
    echo "Network date for $network is $(date -d@$NETWORK_DATE)"
    if [ "$DAY_AGO" -gt "$NETWORK_DATE" ]; then
        # It can happen, that someone got hold node for more than 12 hours,
        # so let's skip removing those ports. Neutron will no allow to
        # remove the network later, if it is in-use.
        echo "Removing ports in DOWN state connected to network: $network" ;
        for port in $(openstack port list --network "$network" -c id -c status -f value | grep -i down | awk '{print $1}'); do
            # NOTE: This step might be useful, when someone remove the trunk network
            # without unseting port from it.
            if ! (openstack port delete "$port"); then
                echo "The port: $port is probably a trunk port. Trying to remove..."
                trunk_network=$(openstack port delete "$port" 2>&1 | grep -o -E "trunk [0-9a-fA-F-]+" | awk '{print $2}')
                echo "Trying to unset port $port from trunk network: $trunk_network"
                openstack network trunk unset "$trunk_network" --subport "$port"
                echo "Trying to remove trunk network: $trunk_network"
                openstack network trunk delete "$trunk_network"
                echo "Trying to remove once again port: $port"
                openstack port delete "$port"
            fi
        done

        echo "Removing subnets for network: $network"
        openstack network list | grep "$network" | awk '{print $6}'  | xargs openstack subnet delete ;
        echo "Removing network: $network"
        openstack network delete "$network"
    fi
done

for port in $(openstack port list | grep -i down | awk '{print $2}'); do
    PORT_DATE=$(date -d "$(openstack port show -c created_at -f value $port)" +%s);
    echo "Port date $port is $(date -d@$PORT_DATE) ($OS_CLOUD)"
    if [ "$DAY_AGO" -gt "$PORT_DATE" ]; then
        echo "Deleting port $port"
        ERROR=$(openstack port delete "$port" 2>&1)
        if [ "$?" -ne 0 ]; then
            if echo "$ERROR" | grep -q "is currently a parent port for trunk"; then
                trunk_network=$(echo "$ERROR" | grep -oP "trunk \K[0-9a-fA-F-]{36}")
                echo "Removing trunk network $trunk_network that belongs to $port"
                openstack network trunk delete "$trunk_network"
                echo "Removing once again port $port"
                openstack port delete "$port"
            fi
        fi;
    fi;
done
