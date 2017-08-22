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

## Namespaces

> Linux namespaces, originally developed by IBM, wrap a set of system resources and present them to a process to make it look like they are dedicated to that process.

## Cgroups

> Linux cgroups, originally developed by Google, govern the isolation and usage of system resources, such as CPU and memory, for a group of processes. For example, if you have an application that takes up a lot of CPU cycles and memory, such as a scientific computing application, you can put the application in a cgroup to limit its CPU and memory usage.

> Namespaces deal with resource isolation for a single process, while cgroups manage resources for a group of processes.

Docker vs LXC

Docker restricts containers to run as a single process. If your application environment consists of X concurrent processes, Docker wants you to run X containers, each with a distinct process. By contrast, LXC containers have a conventional init process and can run multiple processes

> Portability. This is perhaps the single most important advance of Docker over LXC. Docker abstracts away more networking, storage, and OS details from the application than LXC does.


