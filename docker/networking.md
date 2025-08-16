# Networking

https://docs.docker.com/engine/userguide/networking/

```
$ docker network ls
NETWORK ID          NAME                DRIVER              SCOPE
68d2b1b23e04        bridge              bridge              local
a92c5404e7b9        host                host                local
9ff818afc86f        none                null                local
```

When you install docker it creates a network called `bridge`

```
bridge0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
  options=63<RXCSUM,TXCSUM,TSO4,TSO6>
  ether 32:00:11:04:a0:00
  Configuration:
    id 0:0:0:0:0:0 priority 0 hellotime 0 fwddelay 0
    maxage 0 holdcnt 0 proto stp maxaddr 100 timeout 1200
    root id 0:0:0:0:0:0 priority 0 ifcost 0 port 0
    ipfilter disabled flags 0x2
  member: en1 flags=3<LEARNING,DISCOVER>
          ifmaxaddr 0 port 5 priority 0 path cost 0
  nd6 options=201<PERFORMNUD,DAD>
  media: <unknown type>
  status: inactive
```

Docker creates 3 networks by default

1. bridge
    - `docker run ... --network=bridge ...` (or omit `--network=...` entirely)
    - containers join this by default
    - is the `docker0` network interface on the host `ip addr show` to inspect
1. none
    - `docker run ... --network=none ...`
    - any container you add to this network will not have a network inferface
      (`ifconfig` will show loopback only)
1. host
    - `docker run ... --network=host ...`
    - adds the container to the hosts networking stack
    - no network isolation between container and host machine
