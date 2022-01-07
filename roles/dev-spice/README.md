# dev-spice

This role setups the remote desktop spice environment.

Configure the host using this ssh config:

```
Host lambda
     Hostname 10.0.110.5
     User fedora
     LocalForward 59003 localhost:59003
```

Start X server and desktop:

```
/usr/local/bin/spice.sh 3 & sleep 1; /usr/local/bin/desktop.sh
```

Join using virt-manager:

```
remote-viewer spice://127.0.0.1:59003
```
