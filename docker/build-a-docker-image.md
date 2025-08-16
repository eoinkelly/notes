# How to build a docker image

- https://docs.docker.com/engine/reference/builder/
- https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/

- IMPORTANT: The build is run by the Docker daemon, not by the CLI.
- The first thing a build process does is send the entire context (recursively)
  to the daemon.
- In most cases, it’s best to start with an empty directory as context and keep
  your Dockerfile in that directory. Add only the files needed for building the
  Dockerfile.

The Docker daemon runs the instructions in the Dockerfile one-by-one, committing
the result of each instruction to a new image if necessary, before finally
outputting the ID of your new image. The Docker daemon will automatically clean
up the context you sent

Note that each instruction is run independently, and causes a new image to be
created - so `RUN cd /tmp` will not have any effect on the next instructions.

Whenever possible, Docker will re-use the intermediate images (cache), to
accelerate the docker build process significantly.

- This is indicated by the Using cache message in the console output.
- GOTCHA: this also means you have to `apt-get update` and `apt-get install` in
  the same step or the image from the "update" will be cached and not chanaged
  when you change the install command

## Steps

```sh
docker images --all # see what is already installed

docker pull ruby:2.3.1-slim # optional - the build will pull for you if you need it

# build a new image from a Dockerfile in current working dir

# the docker daemon uses the cwd as the "context" for the build so the docker
# client will copy the entire cwd contents to the daemon before build i.e. don't
# build from `/`!!!
docker build -t eoin-ruby-test-1 .

docker history eoin-ruby-test-1       # show layers history
docker inspect eoin-ruby-test-1 |jq . # JSON dump of image metadata

TODO how to clean up layers form images that didn't build properly

# docker run [OPTIONS] IMAGE [COMMAND] [ARG...]
docker run -it --name eoins-container eoin-ruby-test-1 bash
docker run -it --name eoins-container eoin-ruby-test-1 irb
```

TODO: kitematic doesn't seem to show my custom built images
