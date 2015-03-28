#!/bin/bash

echo "I am provisioning"

sudo locale-gen en_NZ.UTF-8

# stop unneeded provisioning services
sudo service puppet stop
sudo service chef-client stop

# Bring Ubuntu 14.04 fully up to date
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install linux-headers-generic linux-headers-virtual linux-image-virtual linux-virtual

# install required packages
sudo apt-get install runit

sudo shutdown -r 0
