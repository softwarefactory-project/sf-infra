# dev-spice

This role setups the remote desktop spice environment.

Configure the host using this ssh config:

```
Host lambda
     Hostname 10.0.110.5
     User fedora
     LocalForward 59003 localhost:59003
```

Join using virt-manager (on your desktop):

```
ssh lambda
remote-viewer spice://127.0.0.1:59003
```

Start X server and desktop (on the lambda host):

```
sudo env SPICE_DEBUG_ALLOW_MC=1 Xspice :3 --port "59003" --disable-ticketing > /dev/null
LD_LIBRARY_PATH= DISPLAY=:3 sh -c "tint2 & /usr/bin/openbox-session"
```
