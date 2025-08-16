# Volumes

## Cheatsheet

```
# find and delete dangling volumes
docker volume ls -f dangling=true
docker volume rm VOLUME_NAME
```

### Data volume containers

- use a container as an abstraction over one or more mounts
- other containers depend on **just the data container name** and they
  automtaically get all the volume mounts from it
    - ++ all depending containers magically get a collection of the exact same
      set of mounts

```
# create a data volume container
# it has two volumes which will be "mounts" of a dir in the linux VM under
# /var/lib/docker/volumes/VOLUME_ID/_data
docker create -v /stuff -v /secret_stuff --name datastore ubuntu /bin/true

# create some dependent containers:
# ++ they get every mount that 'datastore' setup
# ++ they can be deleted and recreated at will and the volume setup will be the same
docker run -it --detach --rm --volumes-from datastore --name my-app-1 ubuntu bash
docker run -it --detach --rm --volumes-from datastore --name my-app-2 ubuntu bash
# ...
```

## Syntax

"bind mount syntax" is

    docker run -v SRC_PATH:DST_PATH

Any SRC_PATH which **begins** with a string which amtches one of the dirs you
have shared from mac to docker then that src will be sourced from your mac.
Every **other** SRC_PATH paths are source from the docker linux distro

/Users /Volumes /tmp /private /

docker run -it -v ~/Desktop:/mydesk ubuntu:latest bash

Paths that already exist in the VM **and** contain files are reserved by Docker
and cannot be exported from macOS i.e. if you have a `/Users` dir in your
container which contains files then you cannot bind mount `/Users` from your mac

## Performance

Explains how it works and the trade-offs:

    https://docs.docker.com/docker-for-mac/osxfs/

## Volume caching options (17.04 and later)

> Docker 17.04 CE Edge adds support for two new flags to the docker run -v,
> --volume option, cached and delegated, that can significantly improve the
> performance of mounted volume access on Docker for Mac.

https://docs.docker.com/docker-for-mac/osxfs-caching/#examples

```
# ensure both host and container see changes at same time
docker run -v /Users/eoinkelly/foo:/foo:consistent

# host's view is authoritive, there may be delay before container sees changes
docker run -v /Users/eoinkelly/foo:/foo:cached

# container is authorative, may be delays before host sees changes
docker run -v /Users/eoinkelly/foo:/foo:delegated
```
