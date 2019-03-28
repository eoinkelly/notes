#!/bin/bash

# everything here runs as root
locale-gen en_NZ.UTF-8

apt update
apt install tree procinfo
