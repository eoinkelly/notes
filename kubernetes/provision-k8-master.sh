#!/bin/bash

yum update -y
yum install -y epel-release
yum install -y htop vim net-tools

# Install docker CE
# #################
# docker-latest doesn't work out of hte box on centos -
# something about overlayfs and selinx not being
# friends.
# yum install -y docker-latest
# systemctl enable docker-latest && systemctl start docker-latest
yum remove docker \
                  docker-common \
                  docker-selinux \
                  docker-engine

yum install -y yum-utils device-mapper-persistent-data lvm2

yum-config-manager \
    --add-repo \
    https://download.docker.com/linux/centos/docker-ce.repo

yum makecache fast

yum install docker-ce -y

systemctl enable docker
systemctl start docker

# Install kubernetes
# ##################

cat <<EOF > /etc/yum.repos.d/kubernetes.repo
[kubernetes]
name=Kubernetes
baseurl=https://packages.cloud.google.com/yum/repos/kubernetes-el7-x86_64
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://packages.cloud.google.com/yum/doc/yum-key.gpg
        https://packages.cloud.google.com/yum/doc/rpm-package-key.gpg
EOF

# Disabling SELinux by running setenforce 0 is required
# to allow containers to access the host filesystem,
# which is required by pod networks for example. You
# have to do this until SELinux support is improved in
# the kubelet
setenforce 0

yum install -y kubelet kubeadm
systemctl enable kubelet && systemctl start kubelet


#############
# setup k8 master

# sudo su

# ??? no idea why but is required ???
# echo 1 > /proc/sys/net/bridge/bridge-nf-call-iptables

# kubeadm init --apiserver-advertise-address=10.0.0.10
