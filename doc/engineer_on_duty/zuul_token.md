How to generate an admin token for Zuul manually
================================================

Reference: https://tree.taiga.io/project/morucci-software-factory/issue/3616

The token is always scoped to ONE tenant. If you need to provide admin access
to several tenants, you must generate a token per tenant.

* ssh into the zuul scheduler
* sudo as root
* use the zuul CLI: `zuul create-auth-token --auth-config zuul_operator --tenant <tenant> --user <user> --expires-in 7776000`
* forward the output of the command to the user through a private medium.

Set user, tenant and expiration as needed.
