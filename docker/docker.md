# Docker

* Is a replacement for virtual machines
* ++ VM host OS takes 10-15% of box resources, docker is much lighter
* ++ much faster startup times
* ++ it has a "dev workflow"
* the "docker images" are immutable and are built up in layers a bit like git
  repo
* you put the mutable things (like logging, app code) on dirs "mounted" in the
  docker image
    * e.g. apache docker image has access to `/var/www` which is mutable and
      also `/var/log` for its logs
* chef automates the setup of VMs
* docker image is a specialized VM designed to run just one app e.g. nginx -
    * it only has as much OS as that app needs
    * saves snapshots of changes so rolling back/forwards is easy
    * => doesn't require something as complex as chef to setup

* docker images can use ubuntu but often use a much smaller distro (busybox)
  that just provides enough to run a single app
    * ++ this makes the images much smaller and quicker to work with
* A `Dockerfile` is a recipe that builds a docker `image`
* The `docker` command loads docker `images` into memory as a docker `container`


### resource usage

* The `default` docker-machine VM seems to run in approx 25MB RAM

### Where things are

The docker installation seems to live in

* on my mac
    * ~/.docker
        * contains the 'default' VM used to host containers on Mac
        * about 85MB after installation on my mac
    * /usr/local/bin
        * /usr/local/bin/docker
        * /usr/local/bin/docker-machine
            * creates and manages "machines" running Docker
        * /usr/local/bin/docker-compose
            * seems to be for building containers
* on the docker host VM
    * all image and container data

### Steps to boot up my docker environment

```
# show docker vms
docker-machine ls

# start VM named "default"
docker-machine start default

# dump shell env vars configuring docker to use that machine
docker-machine env default

# notice no docker variables in the shell environment
env | grep DOCKER

eval "$(docker-machine env default)"

# notice the new variables in the enviornment
env | grep DOCKER


docker images # show images
docker rmi <IMAGENAME> # remove image

docker ps # show running containers
docker ps -a # show running and exited containers
```

# How docker works

TODO - dig into the linux kernel features

# docker tools

1. docker engine
    * runs on linux
1. docker client
    * runs on mac
    * in docker-toolbox
    * under docker github org
1. docker engine
    * in docker-toolbox
    * under docker github org
1. docker-machine
    * in docker-toolbox
    * under docker github org
1. docker-swarm
    * in docker-toolbox
    * under docker github org
1. docker-compose (formerly fig)
    * in docker-toolbox
    * under docker github org
1. Kitematic
    * in docker-toolbox
    * Under kitematic org on github
1. docker-registry
    * under docker github org
    * NOT in toolbox

# Architecture

* https://docs.docker.com/introduction/understanding-docker/

* docker is a client-server architecture - docker client talks to
  docker-daemon (server) on a linux box somewhere
* client and serve communicate via RESTful API

# docker image

* a read-only template for building a containe
* consists of a number of "layers"
* uses a "union filesystem" to combine layers into a single image
* when you build an image you are creating a collection of layers.
  These then get pushed around to CI and production
* When you edit the app and "re-build" you are just adding a new layer
  and only that new layer has to be pushed to CI and production making
  deployments fast
* each instruction in Dockerfile creates a new layer in the image

An app image is built on top of a "base image" which usually represents a particular technology stack e.g. ruby or python

# docker container

* A container consists of
    1. an operating system,
    2. user-added files
    3. meta-data
* NB: when docker runs a container it adds a "read/write layer" to the top of
  the image!

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

# docker client

* runs on my mac
* talks to a _docker daemon_ on a _docker host_ - commands are
  actually executed on the docker-host
* QUESTION: presume it talks to the docker daemon on the docker-host??/

```
# the docker command that starts docker on the boot2docker ISO
/usr/local/bin/docker
-d # run as daemon (this is deprecated, use `docker daemon` instead)
-D # enable debug mode
-g /var/lib/docker
-H unix:// # connect to given socket
-H tcp://0.0.0.0:2376 # connect to given socket
--label provider=virtualbox
--tlsverify # use TLS and verify the remote
--tlscacert=/var/lib/boot2docker/ca.pem # trust TLS certs signed by this CA
--tlscert=/var/lib/boot2docker/server.pem # path to TLS cert file
--tlskey=/var/lib/boot2docker/server-key.pem # path to TLS key file
-s aufs
```

to SSH to the boot2docker VM use

```
env | grep DOCKER # to see IP of VM
ssh docker@<IP OF VM>
# password is: tcuser
```

* The Mac OS X and Windows `docker` binaries are only clients. You cannot use it to run the docker daemon
:
# docker-machine

* It automatically creates hosts, installs Docker on them, then configures the
  docker client to talk to them.
* A “machine” is the combination of a Docker host and a configured client


When you run `docker ps -a` you see all the "containers" that have been defined
on a particular "docker host" (in my case a small linux VM).

* The container info is stored to disk at `/var/lib/docker` on the host
* each "container" on disk is a collection of JSON files that describe it and
  contain its logs etc.
* containers also know which image they are created from

Remove old containers via kitematic or `docker
