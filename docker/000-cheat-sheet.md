# Get a new image from registry

```
# see what is already installed
docker images --all
docker image list

# get image from docker hub
docker pull image-name:tag-name
docker pull registry-host-name-and-optional-port/image-name:tag-name

docker pull registry.access.redhat.com/rhel7
```

# Cleanup

```
# remove all "dangling" images
docker image prune
```

# Create a container

```
# docker run
# -d --detach # detach (don't take over the terminal we run this command in)
# -p --publish CONTAINER_PORT:HOST_PORT # map ports
# -v --volume HOST_PATH:CONTAINER_PATH (TODO: check this)
# --name FOO # name the container
# -t --tty # allocate a tty - where? on host or container? both?
# -i --interactive # keep STDIN open "even if not attached" <-- what?


docker run --name CONTAINER_NAME -d IMAGE_NAME:TAG_NAME

# example: create a container with a specific postgres
docker run --name postgres_9_4_9 -d postgres:9.4.9 -p 5432:5432

# example: postgres with ??? volume setup
docker run -v ~/DockerVolumes/postgres:/var/lib/postgresql/data -p 5432:5432 --name postgres_new2 postgres

docker run --name postgres -d -p 5432:5432 --restart=always postgres:latest
=======

TODO: an example showing postgres volume in diff container

# docker run [OPTIONS] IMAGE [COMMAND] [ARG...]
docker run -it --name eoins-container eoin-ruby-test-1 bash
docker run -it --name eoins-container eoin-ruby-test-1 irb
```

# Build an image

```
# build a new image from a Dockerfile in current working dir

# the docker daemon uses the cwd as the "context" for the build so the docker
# client will copy the entire cwd contents to the daemon before build i.e. don't
# build from `/`!!!
docker build -t eoin-ruby-test-1 .

docker history eoin-ruby-test-1       # show layers history
docker inspect eoin-ruby-test-1 |jq . # JSON dump of image metadata
```
