# Names and tags

- **Images** have a name and tag
- `docker tag` is used to work with both names and tags
- there are two ways to add a name+tag images:
    1. using the docker tag command
    2. passing the -t flag to docker build
- If you omit the tag part docker will automatically insert the `:latest` tag
  name
    - NOTE: the `latest` tag is a naming convention - it does not necessairly
      mean the most recent version of an image - the user has to add it manually
- if you omit the registry hostname and port docker will use
  `registry.docker.io`

{my-docker-hub-username}/{my-docker-hub-repo-name}:{some-tag-name}

Image name format

- is a colleciton of components separated by `/`

```
{optional-registry-hostname-and-port}/{image-name}:{image-tag}
```

the `docker tag` command is used to work with all parts of the name

- it adds a new name+tag to an existing image
- the existing image is found by an existing name+tag

```
docker tag SOURCE_IMAGE[:TAG] TARGET_IMAGE[:TAG]
```
