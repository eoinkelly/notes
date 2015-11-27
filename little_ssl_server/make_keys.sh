#!/bin/bash

# This should match the domain you are trying to spoof
server_fqdn="my.api"

# Create a key and cert in one step
openssl req -newkey rsa:2048 -nodes -keyout domain.key \
  -x509 -days 365 -out domain.crt \
  -subj "/C=NZ/ST=Wellington/L=Wellington/O=Test Company/CN=${server_fqdn}"

