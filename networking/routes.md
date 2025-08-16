There are network interfaces as `ifconfig` shows me The macos networking panel
shows something diff - it is organised by hardware but VPN providers like
tunnelbear are listed there

so to create a vpn client, I guess I need to register a new network interface

so my software appears to be like tunnelbear

`arp` manages the IP<->Ethernet tables held by the OS

`route` command

`routed` daemon - listed in man page but does not exist on macos

```
# linux
sudo route add -net 10.67.0.0/16 gw 192.168.120.254
```

```
# macOS
sudo route -n add -net 10.67.0.0/16  192.168.120.254


# show route table
netstat -nr
```
