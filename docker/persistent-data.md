# Cheat sheet

```
docker run -it -v ~/DockerVolumes/play:/play --name test2 debian /bin/bash
```

Kitematic shows the "in container" path on its container homepage

# Background

Docker has two ways to make data last longer than the container

1. data volume
2. data volume container


# Data volumes

* a designated dir within _one or more_ containers that bypasses the unionfs
* ++ fast: changes are made directly without needing to deal with the layers of the union fs
* data volume persists even if the container is deleted
* data volume can be shared and re-used between containers
* use `docker inspect` to see details of the volume
* data volumes are NOT the same as mounting a directly from the host machine
    * you can optionally mount a directory from the host machine into the volume
* changes to data volumes are made directly (not through unionfs)
* they persist even if the container is deleted

* There is a complication on Mac and Windows because the "docker host" is actually a linux VM.
    * docker mounts `/Users` from the mac into `/Users` on the docker host so you can share dirs from /Users with containers
> Data volumes provide the best and most predictable
> performance. This is because they bypass the storage
> driver and do not incur any of the potential
> overheads introduced by thin provisioning and
> copy-on-write. For this reason, you may want to place
> heavy write workloads on data volumes.


* docker does not automatically delete a volume when you remove a container
* it will also not "garbage collect" volumes that are no longer referenced by any container

* mounting a host (note: linux is the host, not mac) directory is just a special case of creating a volume. Instead of the volume being in something like `/var/lib/docker/volumes/fac362...80535/_data` it is an existing dir on your linux-host.

Where are volumes stored?

* volumes are stored on the linux-host filesystem (not in the unionfs files that the containers use)

How do I see what volumes are on a docker host?


* writing into the container can have perf impacts depending on the storage driver being used by the container

The dockerVM on mac "auto shares" a number of mac filesystem dirs with itself

```
/Users
/Volumes
/tmp
/private
```

# Data volume containers

> If you have some persistent data that you want to share between containers,
> or want to use from non-persistent containers, it’s best to create a named Data
> Volume Container, and then to mount the data from it.

Create a container which has the job of mounting the data volume (from the linux-host) and making that data available to other containers
