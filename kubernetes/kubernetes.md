# Kubernetes

## Setup

### Local

Easiest way to install locally on macOS is Docker Desktop

### Cloud providers

* AWS
    * https://aws.amazon.com/eks/
* Linode
    * https://www.linode.com/products/kubernetes/
* Azure
* GCE

### Cluster management tools

* kops
    * https://kops.sigs.k8s.io/
    * a tool to manage clusters, supports AWS. DigitalOcean,OpenStack in beta, Azure, GCE in alpha

## kubectl cheat sheet

* kubectl
    * controls the Kubernetes cluster manager.
    * seems to wnat to connect to localhost:8080 by default

```bash
# starter commands
kubectl version
# Client Version: version.Info{Major:"1", Minor:"22", GitVersion:"v1.22.5", GitCommit:"5c99e2ac2ff9a3c549d9ca665e7bc05a3e18f07e", GitTreeState:"clean", BuildDate:"2021-12-16T08:38:33Z", GoVersion:"go1.16.12", Compiler:"gc", Platform:"darwin/arm64"}
# Server Version: version.Info{Major:"1", Minor:"22", GitVersion:"v1.22.5", GitCommit:"5c99e2ac2ff9a3c549d9ca665e7bc05a3e18f07e", GitTreeState:"clean", BuildDate:"2021-12-16T08:32:32Z", GoVersion:"go1.16.12", Compiler:"gc", Platform:"linux/arm64"}

kubectl cluster-info
# Kubernetes control plane is running at https://kubernetes.docker.internal:6443
# CoreDNS is running at https://kubernetes.docker.internal:6443/api/v1/namespaces/kube-system/services/kube-dns:dns/proxy

# To further debug and diagnose cluster problems, use 'kubectl cluster-info dump'.

kubectl cluster-info dump | bat -l json
# extremely long output

kubectl get nodes
```

## Long term data storage

* use a StatefulSet workload
    * manages a set of Pods
    * provides guarantees about the order and uniqueness of the Pods
    * Pods have a unique persistent ID and a stable hostname

* PersistenceVolume
    * https://kubernetes.io/docs/concepts/storage/persistent-volumes/
    * an abstraction over how the volume is actually implemented
        * https://kubernetes.io/docs/concepts/storage/persistent-volumes/#types-of-persistent-volumes
    * a k8s API object
    * a Pod can have a claim on the volume
    * can specify **node affinity** to define constraints that limit what nodes this volume can be accessed from. Pods that use a PV will only be scheduled to nodes that are selected by the node affinity
        * e.g. node affinity of a local filesystem path would restrict the pod to a single node
        * but other file storage technologies could be different
    * sets capacity,access modes and path on host where the data exists

## Exams


1. CKAD Certified K8s Application Developer
    * recommended to do this one first
1. CKA Certified K8s Application Administrator
    * recommended as second course

## Architecture

### Runtime

* requires a container runtime on each node that conforms to Container Runtime Interface (CRI)
* supported CRI runtimes

1. containerd
    * de-facto standard
    * created by docker
    * used directly by docker
    * used by k8s through a `cri` plugin which makes it CRI compatible
    * https://containerd.io/
1. CRI-O
    * created by Redhat for k8s
    * slimmer than containerd
    * used by openshift
1. Docker engine
1. Mirantis (formerly Docker Enterprise)

The runtimes usually have a low-level runtime within them which uses

* runC
    * maintained as part of Moby by Docker
    * de-facto default
* crun
* rkt (deprecated)

### Etcd

* See [etcd notes](./etcd.md)

### Operators

> Operators are software extensions to Kubernetes that make use of custom
> resources to manage applications and their components. Operators follow
> Kubernetes principles, notably the control loop

* let you extend the k8s API by creating custom objects within your cluster
* https://kubernetes.io/docs/concepts/extend-kubernetes/operator/#motivation
* https://developers.redhat.com/articles/2021/06/11/kubernetes-operators-101-part-1-overview-and-key-features
* k8s comes with some controllers which watch for a diff between desired and actual state and then reconcile
    * operators let you create your own controllers
    * both kinds of controller are run as part of k8s control loop
* operators and helm charts broadly achieve the same things
    * ++ helm: good for simpler applications
    * ++ operators: good for complex applications
* a way to script what a human operator would do to responds to events happening to an application e.g. repeatable tasks
* e.g.
  * backup app state
  * upgrade the app
  * tell apps that don't understand k8s about some event e.g. a deploy
  * simulate failure in all or part of the cluster
* are apps which use the k8s API
* they can be deployed in containers within your cluster or outside it
* You can build an operator with Helm Charts, Ansible playbooks, or Golang

1. Helm charts
    * ++ simple
    * -- limited
2. Ansible playbooks
    * ++ good if you don't want to write golang but need more than helm charts can do
3. Golang
    * ++ most powerful but also most coding work


### master node

> The Master is responsible for managing the cluster. The master coordinates all
> activities in your cluster, such as scheduling applications, maintaining
> applications' desired state, scaling applications, and rolling out new updates.

> A node is a VM or a physical computer that serves as a worker machine in a
> Kubernetes cluster. Each node has a Kubelet, which is an agent for managing the
> node and communicating with the Kubernetes master. The node should also have
> tools for handling container operations, such as Docker or rkt. A Kubernetes
> cluster that handles production traffic should have a minimum of three nodes.

> The nodes communicate with the master using the Kubernetes API, which the
> master exposes. End users can also use the Kubernetes API directly to interact
> with the cluster.

### Helm (package management)

> Helm is a package manager for Kubernetes. Helm is the K8s equivalent of yum or
> apt. Helm deploys charts, which you can think of as a packaged application. It
> is a collection of all your versioned, pre-configured application resources
> which can be deployed as one unit.

* helm chart = the package
    * seems to be a collection of yaml files
    * examples:
        * https://artifacthub.io/packages/helm/bitnami/redis
* helm and operators kind of do the same thing. Helm is better for simpler situations