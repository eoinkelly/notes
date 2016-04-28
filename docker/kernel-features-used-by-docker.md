# kernel features

Docker uses certain features of the linux kernel

1. namespaces
    * linux kernel provides separate namespaces for things like
        * pids
        * network interfaces
        * mount points
        * IPC
2. control groups
    * allows docker to share hardware resources between containers and
      optionally setup limits on those
3. union filesystem
    * there are a no. of options:
        * aufs
        * btrfs
        * vfs
        * DeviceMapper

Docker combines the features above into a wrapper called a "container format"

There are multiple container formats

* libcontainer (docker default)
* BSD jail
* LXC (traditional linux container)
* Solaris zone

