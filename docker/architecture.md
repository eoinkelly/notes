# Architecture

- https://docs.docker.com/introduction/understanding-docker/

- docker is a client-server architecture - docker client talks to docker-daemon
  (server) on a linux box somewhere
- client and serve communicate via RESTful API

It seems docker images are kept on the docker server i.e. on the linux vm not my
mac

=> each docker amchine has its own collection of images

# Architecture pt 1: images

QUESTION: where are images stored on disk?

- a read-only template for building a containe
- consists of a number of "layers"
- uses a "union filesystem" to combine layers into a single image
- when you build an image you are creating a collection of layers. These then
  get pushed around to CI and production
- When you edit the app and "re-build" you are just adding a new layer and only
  that new layer has to be pushed to CI and production making deployments fast
- each instruction in Dockerfile creates a new layer in the image

An app image is built on top of a "base image" which usually represents a
particular technology stack e.g. ruby or python

# Architecture pt 2: containers

- A container consists of
    1. an operating system,
    2. user-added files
    3. meta-data
- NB: when docker runs a container it adds a "read/write layer" to the top of
  the image!

# Architecture pt 3: registries

TODO
