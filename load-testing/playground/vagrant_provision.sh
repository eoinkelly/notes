#!/bin/bash

# vagrant provisioners run as root and we want our script to run as the vagrant
# user so we have this wrapper.

sudo --set-home -u ubuntu bash /vagrant/server_setup.sh
