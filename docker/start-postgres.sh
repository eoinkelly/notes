#!/bin/bash

# create a postgres container of the specific version
# -p PORT_ON_HOST:PORT_IN_CONTAINER
docker run -d \
           --log-opt max-size=100m \
           --log-opt max-file=1 \
           --restart always \
           -p 5433:5432 \
           -e POSTGRES_PASSWORD=mysecretpassword \
           --name postgres_9_6_0 \
           postgres:9.6.0

# connect to the new postgres (it will prompt for
# password - see env var set above)
# psql -h localhost -p 5433 -U postgres

# create a container using the postgres image and run psql in it to connect to
# the container running the postgres server
# docker run -it --rm --link POSTGRES_SERVER_CONTAINER_NAME postgres psql -h POSTGRES_SERVER_CONTAINER_NAME -U postgres
