#!/bin/bash

# Instructions:
# https://container-registry.oracle.com/

# oracle requires shared mem of 4g, 8g is recommended
docker run -it \
  --env-file ./oracle.env \
  -p 1521:1521 \
  -p 5500:5500 \
  --name eoin-oracle-docker \
  --shm-size="4g" \
  container-registry.oracle.com/database/standard
