# Union file system

> Union file systems allow files and directories of separate file systems, known as branches, to be transparently overlaid, forming a single coherent file system.

https://en.wikipedia.org/wiki/UnionFS

* used by linux live cds
* branches can be read-only or read-write
* a write actually copies from a read-only layer onto a higher priority writable layer which is then mutated
* docker uses unionfs


## unionfs

* available for linux, freebsd, netbsd
* implements a "union mount" for _other filesystems_
* each layer of the filesystem is called a "branch"


Alternatives

* aufs
* overlayfs
    * used in openwrt
    * is in mainline linux kernel since 2014

* plan 9 uses union mounts a lot
* gnu hurd has a unionfs implementation

## aufs

* aufs (short for advanced multi layered unification filesystem) implements a union mount for Linux file systems.
* aufs has not been merged into linux mainline kernels - code was too dense and unreadable
    * overlayfs was merged in instead
* a rewrite of unionfs
* many livecds have chosen aufs instead of unionfs
* docker uses aufs ???

# Docker storage backends

https://docs.docker.com/engine/userguide/storagedriver/selectadriver/

* docker daemon can only used on storage driver at a time ( => all containers must use the same one)
* docker supports many storage drivers
    * overlayfs
    * aufs
    * btrfs
    * devicemapper
    * zfs
    * vfs
* use `docker info` to see which storage driver is set
* The `Backing Filesystem` is the filesystem that is being layered/unioned by the storage driver
    * this is the fs used to create the docker host's "local storage area" under `/var/lib/docker`
AUFS is only the default storage back end on systems/distributions that have it available. Otherwise, devicemapper is the default. Ubuntu 14.04, for example, defaults to devicemapper

This changed in Docker 0.7.0. Prior to 0.7.0, Docker relied upon AUFS as its only storage driver, which is why it was the default in earlier versions of Ubuntu.

discourse strongly recommend not using devicemapper for docker - they recommend aufs

