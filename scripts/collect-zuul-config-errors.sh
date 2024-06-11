# Copyright (C) 2024 Red Hat
# SPDX-License-Identifier: Apache-2.0
#
# Write to one file per tenants all the config-errors.
#
ZUUL_API=${1:-localhost:9000}
for tenant in $(curl $ZUUL_API/api/tenants | jq -r ".[] | .name"); do
    curl $ZUUL_API/api/tenant/$tenant/config-errors | jq -r ".[]" > $tenant
done
