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

eval $(docker-machine env default)

# notice the new variables in the enviornment
env | grep DOCKER


docker images # show images
docker rmi <IMAGENAME> # remove image

docker ps # show running containers
docker ps -a # show running and exited containers
```

# 8 Docker tools

1. docker engine
    * runs on linux
2. docker client
    * runs on mac/windows/linux
    * in docker-toolbox
3. docker engine
    * in docker-toolbox
4. docker-machine
    * in docker-toolbox
5. docker-swarm
    * in docker-toolbox
6. docker-compose (formerly fig)
    * in docker-toolbox
7. Kitematic
    * in docker-toolbox
8. docker-registry
    * NOT in toolbox

# 2. docker client

* runs on my mac
* talks to a _docker daemon_ on a _docker host_ - commands are
  actually executed on the docker-host

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
s
Task: SSH to the boot2docker VM:

* Option: `docker-machine ssh default`
* Option:
    ```
    env | grep DOCKER # to see IP of VM
    ssh docker@<IP OF VM>
    # password is: tcuser
    ```

* The Mac OS X and Windows `docker` binaries are clients only. You cannot use
them to run the docker daemon

# docker-machine

* you can use docker-machine to automate creating VMs with docker pre-configured  on AWS, digital ocean etc.
    * each cloud provider is a "driver" from `docker-machine` pov
    * => i can use exactly the same workflow to setup a VM in the cloude and a virtualbox VM on my local box!
* available drivers
    * cloud: aws, azure, digitalocean
    * local: virtualbox, hyper-v, vmware fusion
* The `default` machine name is special - if you don't specify a machine name docker-machine will assume it should use the `default` machine

* you can add a machien that is just a URL (not driver involved)
    * you don't get the provisioning magic but you avoid having to type the full url when issuing commands

When you run `docker ps -a` you see all the "containers" that have been defined
on a particular "docker host" (in my case a small linux VM).

* The container info is stored to disk at `/var/lib/docker` on the host
* each "container" on disk is a collection of JSON files that describe it and
  contain its logs etc.
* containers also know which image they are created from

To remove old containers

* Option 1: via kitematic
* Option 2: `docker ???`

Allows you to upgrade a machine

```
# docker-machine upgrade default
# basically does:
Copying /Users/eoinkelly/.docker/machine/cache/boot2docker.iso to /Users/eoinkelly/.docker/machine/machines/default/boot2docker.iso...
```


### base images

* docker-machine supports a bunch of linux distro base images but its defaults are

* boot2docker for local installs (virtualbox, hyper-v vmware fusino etc.)
* the latest Ubuntu LTS that the provider supports (currently ubuntu 12.04+) for cloud installs (AWS, digitalocean etc.)


### How is docker-machine vs chef

* both allow me to provision a machine
* docker-machine is much more simplistic - it lets me install a base image and setup docker
    * ??? can you install other software on it via docker-machine ???
* in theory if the vm just has a base image + docker server (i.e. all
interesting software is in containers) then there shouldn't be much for chef to
do

* It automatically creates hosts, installs Docker on them, then configures the
  docker client to talk to them.
* A “machine” is the combination of a Docker host and a configured client

