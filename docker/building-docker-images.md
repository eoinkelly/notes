--squash lets you squash all layers down to a single layer

docker images have a "digest" as well as a tag

Build time args

- don't put secrets in buildtime args because they can be seen later via the
  `docker history`
- you can pass build args to docker which are consumed by the Dockerfile and can
  change its behaviour
- build args are not persisted in the build image - use ENV vars if you need
  this
    - you can set the value of an ENV var from an ARG var so you can set
      environment variables in the container based on build-time arg
