Regenerating rdo registry token
===============================

Why is it not ok
----------------

The token for Openshift might be expired or the Openshift cluster
cert(s) can be changed, so the token will not work.

Solution
--------

## WARNING!

It might happen that the registry certificate regeneration playbook that is
running on the bridge host fails somewhere. Before going to the next steps,
check the logs in `/var/log/registry-cert-renew/`.


You need to regenerate new token, but before that, ensure that
old are not working.

On registry host:
```
oc describe sa 'tripleo.service'
```

Then get the old token:
```
oc get secret tripleo.service-token-mytoken -o yaml
```

After that try to use the token, e.g.:
```
podman login trunk.registry.rdoproject.org -u some_user_it_does_not_metter -p<token>
```

If you have: "invalid username/password" probably your token has been removed.

To create new token, do:
```
oc sa get-token 'tripleo.service'
```

The new generated token, put into the file and regenerate the secret
with zuul encrypt tool, e.g.:
```
git clone https://opendev.org/zuul && cd zuul
cat << EOF > token
<token>
EOF

./tools/encrypt_secret.py --tenant rdoproject.org --infile token --outfile token.out https://review.rdoproject.org/zuul/ config
```

The encrypted token put into the zuul secrets file.
