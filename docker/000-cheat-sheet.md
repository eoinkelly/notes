
```
# docker run
# -d --detach # detach (don't take over the terminal we run this command in)
# -p --publish CONTAINER_PORT:HOST_PORT # map ports
# -v --volume HOST_PATH:CONTAINER_PATH (TODO: check this)
# --name FOO # name the container
# -t --tty # allocate a tty
# -i --interactive # keep STDIN open "even if not attached" <-- what?


docker run --name CONTAINER_NAME -d IMAGE_NAME:TAG_NAME

# example: create a container with a specific postgres
docker run --name postgres_9_4_9 -d postgres:9.4.9 -p 5432:5432

# example: postgres with ??? volume setup
docker run -v ~/DockerVolumes/postgres:/var/lib/postgresql/data -p 5432:5432 --name postgres_new2 postgres
```
