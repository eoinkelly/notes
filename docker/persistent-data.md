Two ways to make data last longer than the container

1. data volume
2. data volume container


# Data volumes

* a designated dir within _one or more_ containers that bypasses the unionfs
* ++ fast: changes are made directly without needing to deal with the layers of the union fs
* data volume persists even if the container is deleted
* data volume can be shared and re-used between containers
* use `docker inspect` to see details of the volume
* you can optionally mount a directory from the host machine into the volume

There is a complication on Mac and Windows because the "docker host" is actually a linux VM.
* docker mounts `/Users` from the mac into `/Users` on the docker host so you can share dirs from /Users with containers
> Data volumes provide the best and most predictable
> performance. This is because they bypass the storage
> driver and do not incur any of the potential
> overheads introduced by thin provisioning and
> copy-on-write. For this reason, you may want to place
> heavy write workloads on data volumes.

* writing into the container can have perf impacts depending on the storage
  driver being used by the container


# Data volume containers
