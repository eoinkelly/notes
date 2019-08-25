# Kubernetes


## Installation

```
brew install cfssl # installs cfssl, cfssljson
brew install kubectl # if you need it
```

## General points

Getting started in a cloud provider seems best.

AWS
* there are a few options which are ContainerLinux only
* Other options are
    * kops
    * conjure-up
* Ubuntu seems to be marketing their "first class citizen" status in k8s world

## Misc notes

Helm - package management

Etcd - secrets management
    etcd operator

> The Master is responsible for managing the cluster. The master coordinates all activities in your cluster, such as scheduling applications, maintaining applications' desired state, scaling applications, and rolling out new updates.

> A node is a VM or a physical computer that serves as a worker machine in a Kubernetes cluster. Each node has a Kubelet, which is an agent for managing the node and communicating with the Kubernetes master. The node should also have tools for handling container operations, such as Docker or rkt. A Kubernetes cluster that handles production traffic should have a minimum of three nodes.

> he nodes communicate with the master using the Kubernetes API, which the master exposes. End users can also use the Kubernetes API directly to interact with the cluster.

kubectl
    controls the Kubernetes cluster manager.
    seems to wnat to connect to localhost:8080 by default

```
kubectl version
kubectl get nodes
kubectl cluster-info
```


How do I setup a centos box to be a k8 master?
https://kubernetes.io/docs/setup/independent/create-cluster-kubeadm/

## Kubernetes the hard way

The following components need a TLS certificate to establish their identity

1. etcd
1. kube-apiserver
1. kube-controller-manager
1. kube-scheduler
1. kubelet
1. kube-proxy
