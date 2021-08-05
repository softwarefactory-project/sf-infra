# How to add a vault secret

To manage vault secret, you need to connect to the bridge, then as fedora user:

```
ansible-vault encrypt_string --stdin-name var-name < file-var-value >> var-file.yaml
```

Alternative way:

Encrypt:
```
db_password: test
```

can be done:

```
echo -n 'test' | ansible-vault encrypt_string  --stdin-name db_password
```
