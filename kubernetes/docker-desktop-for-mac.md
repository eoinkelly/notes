# Docker desktop for mac

## Access the linux VM running docker

This works as of Feb 2022.

```bash
# the only way to exit is to close the terminal window as the prompt is configured to auto login

# login
socat ~/Library/Containers/com.docker.docker/Data/vms/0/console.sock -,rawer
```

## filesystem perf on macos

- https://mobile.twitter.com/marcan42/status/1494213855387734019
- https://mjtsai.com/blog/2022/02/17/apple-ssd-benchmarks-and-f_fullsync/
- https://twitter.com/rosyna/status/1494248499067514883
- https://github.com/docker/roadmap/issues/7
    - has a command you can run in the moby vm to tell linux to not issue the
      command to the disk to write cache to stable storage
    - this apparently really helps when you are running imports/exports of data
      from an RDBMS
    - I think we usually avoid this by just keeping the data in a volume or in
      the container

- linux fsync
    - how it works:
        1. flush writes to the drive
        2. ask drive to flush its write cache to stable storage
    - has 46 IOPS when it should be of the order of 40K IOPS
    - lots of linux software written to assume tha fsync makes your data safe

- macos fsync
    - how it works
        1. flush writes to the drive
    - F_FULLSYNC operation to ask drive to flush its write cache to stable
      storage using F_FULLSYNC is **very** slow
    - If you run without F_FULLSYNC then you might lose data if the kernel
      panics or power is suddenly removed (less of a problem in laptops but real
      for desktops)
    - Apple's drives have faster IOPS perf when compared without flushing but
      with flushing turned on they are the slowest of what he compared

I'm not sure whether the above discussion impacts the PHP use case or just the
RDBMS use case?

Docker desktop uses gRPC FUSE

## Overlay filesystem

https://docs.docker.com/storage/storagedriver/select-storage-driver/

Docker images can use a variety of overlay filesystems:

- `overlay2`
    - preferred and the default
- `aufs`
    - deprecated, recommend overlay2 instead
    - was default until docker 18.06
- `overlay`
    - legacy version of `overlay2` which supports old linux kernels
    - deprecated and will be removed in later release of Docker
        - what about whatever k8s uses?

Overlay2 needs 4 paths on the host filesystem:

1. _LowerDir_
    - this is the read-only layer
    - it can be built from a number of "diff"s (see below)
    - presumably each diff represents a layer built into the docker image
1. _UpperDir_
    - changes made by the running container/OS will be made on this layer
1. _WorkDir_
    - an internal temp working dir for overlay2
    - you must specify it but it shouldn't have existing files
1. _MergedDir_
    - this is where the merged view of the filesystem will be available

```bash
❯ docker inspect memcached-1 | jq '.[].GraphDriver'
{
  "Data": {
    "LowerDir": "/var/lib/docker/overlay2/4466da9f86fc9c82e8b306c75c779c91bcfeb2778a6c870d40845918e7a8b345-init/diff:/var/lib/docker/overlay2/884d148465e988f3be4d099a2d2b59af54d910dce31138e0906017e3db399cbe/diff:/var/lib/docker/overlay2/7a1919b0b71f43c1956f60c9d8d06884bd7a423227296a224ecb427ed9e13535/diff:/var/lib/docker/overlay2/d8db481937955fad4ad359dbf97ca23824ac8c4bee39f0057c607ca790c4592e/diff:/var/lib/docker/overlay2/0a24798174b8a7988a82a933dd86cea47b425510bb04898827b39f3ac91e7c00/diff:/var/lib/docker/overlay2/4c2980a41b0b8e083a674bc4511734aa9a7982940d3100d0268da6759eb52fec/diff:/var/lib/docker/overlay2/84c30e37ac6eb26e4ef8ea2f994f7f0a6bb264cd24d30d58f242b3a08caac27a/diff",
    "MergedDir": "/var/lib/docker/overlay2/4466da9f86fc9c82e8b306c75c779c91bcfeb2778a6c870d40845918e7a8b345/merged",
    "UpperDir": "/var/lib/docker/overlay2/4466da9f86fc9c82e8b306c75c779c91bcfeb2778a6c870d40845918e7a8b345/diff",
    "WorkDir": "/var/lib/docker/overlay2/4466da9f86fc9c82e8b306c75c779c91bcfeb2778a6c870d40845918e7a8b345/work"
  },
  "Name": "overlay2"
}

# no
"LowerDir":
"/var/lib/docker/overlay2/4466da9f86fc9c82e8b306c75c779c91bcfeb2778a6c870d40845918e7a8b345-init/diff
/var/lib/docker/overlay2/884d148465e988f3be4d099a2d2b59af54d910dce31138e0906017e3db399cbe/diff
/var/lib/docker/overlay2/7a1919b0b71f43c1956f60c9d8d06884bd7a423227296a224ecb427ed9e13535/diff
/var/lib/docker/overlay2/d8db481937955fad4ad359dbf97ca23824ac8c4bee39f0057c607ca790c4592e/diff
/var/lib/docker/overlay2/0a24798174b8a7988a82a933dd86cea47b425510bb04898827b39f3ac91e7c00/diff
/var/lib/docker/overlay2/4c2980a41b0b8e083a674bc4511734aa9a7982940d3100d0268da6759eb52fec/diff
/var/lib/docker/overlay2/84c30e37ac6eb26e4ef8ea2f994f7f0a6bb264cd24d30d58f242b3a08caac27a/diff",

❯ docker inspect mysql-1 | jq '.[].GraphDriver'
{
  "Data": {
    "LowerDir": "/var/lib/docker/overlay2/6fb08b28849169ca9814b517ecb21c1da8232c064e9b59b63f0fe135ba998628-init/diff:/var/lib/docker/overlay2/8d3ee63a30b18f160538d4000bae6865ac5818287bf282edfac31d063f7f91f1/diff:/var/lib/docker/overlay2/3587d669b24230c069a49b63fb3c994380a2a5ecd43849c49f0b9ee43dc2f5a3/diff:/var/lib/docker/overlay2/2e5e4cdf2c668904e5f36a9bc8900121714fd63cd02477f5ed912c1cb2c5fd20/diff:/var/lib/docker/overlay2/3e9b195bf8b78ad2005b3985d4d53ae520a3eae1f15f5fb487abefff4dfad1cc/diff:/var/lib/docker/overlay2/5f767cf7cdd675013aed2340edff26f91276cfd8de40faaa4b9de7453c3d37b2/diff:/var/lib/docker/overlay2/a964864028f7b4a2d403cd548c50335554ae70982449054a5d06380b325a33f7/diff:/var/lib/docker/overlay2/a8acba7cd1f97f9be2291b8cf61980c60f5ef176d0b18f8089cc61f5b782e858/diff:/var/lib/docker/overlay2/fc98abb7d338014688654342d504cf75e3416ae58981a4fb54ac311b64865ba5/diff:/var/lib/docker/overlay2/57ba1a1536129474ad66a99e14da82f2d5a393273da0c9e0f84ba844117eb7df/diff:/var/lib/docker/overlay2/8e3c670d56951d5cc328da2efb992517b85fabea7b7195d338b62882c595f2d1/diff:/var/lib/docker/overlay2/b7d2128a66f7059127b11d4d91a2fb2f0c48d21428f8b553625cfa41579c0496/diff",
    "MergedDir": "/var/lib/docker/overlay2/6fb08b28849169ca9814b517ecb21c1da8232c064e9b59b63f0fe135ba998628/merged",
    "UpperDir": "/var/lib/docker/overlay2/6fb08b28849169ca9814b517ecb21c1da8232c064e9b59b63f0fe135ba998628/diff",
    "WorkDir": "/var/lib/docker/overlay2/6fb08b28849169ca9814b517ecb21c1da8232c064e9b59b63f0fe135ba998628/work"
  },
  "Name": "overlay2"
}

# expanding LowerDir from above
"LowerDir"
 "/var/lib/docker/overlay2/6fb08b28849169ca9814b517ecb21c1da8232c064e9b59b63f0fe135ba998628-init/diff
/var/lib/docker/overlay2/8d3ee63a30b18f160538d4000bae6865ac5818287bf282edfac31d063f7f91f1/diff
/var/lib/docker/overlay2/3587d669b24230c069a49b63fb3c994380a2a5ecd43849c49f0b9ee43dc2f5a3/diff
/var/lib/docker/overlay2/2e5e4cdf2c668904e5f36a9bc8900121714fd63cd02477f5ed912c1cb2c5fd20/diff
/var/lib/docker/overlay2/3e9b195bf8b78ad2005b3985d4d53ae520a3eae1f15f5fb487abefff4dfad1cc/diff
/var/lib/docker/overlay2/5f767cf7cdd675013aed2340edff26f91276cfd8de40faaa4b9de7453c3d37b2/diff
/var/lib/docker/overlay2/a964864028f7b4a2d403cd548c50335554ae70982449054a5d06380b325a33f7/diff
/var/lib/docker/overlay2/a8acba7cd1f97f9be2291b8cf61980c60f5ef176d0b18f8089cc61f5b782e858/diff
/var/lib/docker/overlay2/fc98abb7d338014688654342d504cf75e3416ae58981a4fb54ac311b64865ba5/diff
/var/lib/docker/overlay2/57ba1a1536129474ad66a99e14da82f2d5a393273da0c9e0f84ba844117eb7df/diff
/var/lib/docker/overlay2/8e3c670d56951d5cc328da2efb992517b85fabea7b7195d338b62882c595f2d1/diff
/var/lib/docker/overlay2/b7d2128a66f7059127b11d4d91a2fb2f0c48d21428f8b553625cfa41579c0496/diff",
```

## Kubernetes

Docker desktop can setup k8s

- Installing docker for mac
    - installs `kubectl`
- Enabling kubernetes in docker for mac does the following:
    - Starts 18 containers
    - Causes docker for mac to use approx 4.15 GB memory on my laptop ...

```
# image     start-command
1. docker/kube-compose-controller  "/compose-controller --kubeconfig --reconciliation-interval 30s"
2. k8s.gcr.io/kube-proxy-amd64    "/usr/local/bin/kube-proxy --config=/var/lib/kube-proxy/config.conf"
3. docker/kube-compose-api-server  "/api-server --kubeconfig --authentication-kubeconfig --authorization-kubeconfig --etcd-servers=https://127.0.0.1:2379 --etcd-cafile=/etc/docker-compose/etcd/ca.crt --etcd-certfile=/etc/docker-compose/etcd/client.crt --etcd-keyfile=/etc/docker-compose/etcd/client.key --secure-port=9443 --tls-cert-file=/etc/docker-compose/tls/server.crt --tls-private-key-file=/etc/docker-compose/tls/server.key"
4. k8s.gcr.io/pause-amd64:3.1   "/pause"
5. k8s.gcr.io/pause-amd64:3.1   "/pause"
6. k8s.gcr.io/pause-amd64:3.1   "/pause"
7. k8s.gcr.io/kube-scheduler-amd64   "kube-scheduler --address=127.0.0.1 --leader-elect=true --kubeconfig=/etc/kubernetes/scheduler.conf"
8. k8s.gcr.io/pause-amd64:3.1   "/pause"
9. k8s.gcr.io/kube-controller-manager-amd64 "kube-controller-manager --address=127.0.0.1 --leader-elect=true --controllers=*,bootstrapsigner,tokencleaner --kubeconfig=/etc/kubernetes/controller-manager.conf --root-ca-file=/run/config/pki/ca.crt --cluster-signing-cert-file=/run/config/pki/ca.crt --cluster-signing-key-file=/run/config/pki/ca.key --use-service-account-credentials=true --service-account-private-key-file=/run/config/pki/sa.key"
10. k8s.gcr.io/pause-amd64:3.1   "/pause"
11. k8s.gcr.io/kube-apiserver-amd64   "kube-apiserver --admission-control=Initializers,NamespaceLifecycle,LimitRanger,ServiceAccount,DefaultStorageClass,DefaultTolerationSeconds,NodeRestriction,MutatingAdmissionWebhook,ValidatingAdmissionWebhook,ResourceQuota --requestheader-group-headers=X-Remote-Group --requestheader-extra-headers-prefix=X-Remote-Extra- --requestheader-allowed-names=front-proxy-client --kubelet-client-certificate=/run/config/pki/apiserver-kubelet-client.crt --proxy-client-cert-file=/run/config/pki/front-proxy-client.crt --insecure-port=0 --enable-bootstrap-token-auth=true --proxy-client-key-file=/run/config/pki/front-proxy-client.key --allow-privileged=true --requestheader-username-headers=X-Remote-User --secure-port=6443 --client-ca-file=/run/config/pki/ca.crt --tls-private-key-file=/run/config/pki/apiserver.key --requestheader-client-ca-file=/run/config/pki/front-proxy-ca.crt --kubelet-preferred-address-types=InternalIP,ExternalIP,Hostname --service-cluster-ip-range=10.96.0.0/12 --service-account-key-file=/run/config/pki/sa.pub --advertise-address=192.168.65.3 --tls-cert-file=/run/config/pki/apiserver.crt --kubelet-client-key=/run/config/pki/apiserver-kubelet-client.key --authorization-mode=Node,RBAC --etcd-servers=https://127.0.0.1:2379 --etcd-cafile=/run/config/pki/etcd/ca.crt --etcd-certfile=/run/config/pki/apiserver-etcd-client.crt --etcd-keyfile=/run/config/pki/apiserver-etcd-client.key"
12. k8s.gcr.io/pause-amd64:3.1   "/pause"
13. sha256:6f7f2dc7fab5d7e7f99dc4ac176683a981a9ff911d643b9f29ffa146838deda3 "/sidecar --v=2 --logtostderr --probe=kubedns,127.0.0.1:10053,kubernetes.default.svc.cluster.local,5,SRV --probe=dnsmasq,127.0.0.1:53,kubernetes.default.svc.cluster.local,5,SRV"
14. sha256:c2ce1ffb51ed60c54057f53b8756231f5b4b792ce04113c6755339a1beb25943 "/dnsmasq-nanny -v=2 -logtostderr -configDir=/etc/k8s/dns/dnsmasq-nanny -restartDnsmasq=true -- -k --cache-size=1000 --no-negcache --log-facility=- --server=/cluster.local/127.0.0.1#10053 --server=/in-addr.arpa/127.0.0.1#10053 --server=/ip6.arpa/127.0.0.1#10053"
15. sha256:80cc5ea4b547abe174d7550b82825ace40769e977cde90495df3427b3a0f4e75 "/kube-dns --domain=cluster.local. --dns-port=10053 --config-dir=/kube-dns-config --v=2"
16. k8s.gcr.io/pause-amd64:3.1  "/pause"
17. sha256:52920ad46f5bf730d0e35e11215ec12d04ca5f32835f2ceb8093d0bd38930735 "etcd --listen-client-urls=https://127.0.0.1:2379 --trusted-ca-file=/run/config/pki/etcd/ca.crt --peer-cert-file=/run/config/pki/etcd/peer.crt --peer-trusted-ca-file=/run/config/pki/etcd/ca.crt --advertise-client-urls=https://127.0.0.1:2379 --client-cert-auth=true --peer-client-cert-auth=true --data-dir=/var/lib/etcd --cert-file=/run/config/pki/etcd/server.crt --key-file=/run/config/pki/etcd/server.key --peer-key-file=/run/config/pki/etcd/peer.key"
18. k8s.gcr.io/pause-amd64:3.1  "/pause"
```

Why so many "pause containers

> Every Kubernetes Pod includes an empty pause container, which bootstraps the
> pod to establish all of the cgroups, reservations, and namespaces before its
> individual containers are created. The pause container image is always
> present, so the pod resource allocation happens instantaneously as containers
> are created.

It creates the following containers:

    1 x kube-controller-manager
    1 x kube-scheduler
    1 x kube-apiserver
    1 x etcd
    1 x kube-vpnkit-forward
    1 x docker/desktop-vpnkit-controller
    1 x docker/desktop-storage-provisioner
    1 x storage-provisioner_kube-system
    1 x k8s_coredns
    2 x k8s_POD_coredns
    9 x pause

```
# ~/.kube/config
apiVersion: v1
clusters:
- cluster:
    insecure-skip-tls-verify: true
    server: https://localhost:6443
  name: docker-for-desktop-cluster
contexts:
- context:
    cluster: docker-for-desktop-cluster
    user: docker-for-desktop
  name: docker-for-desktop
current-context: docker-for-desktop
kind: Config
preferences: {}
users:
- name: docker-for-desktop
  user:
    client-certificate-data: <redacted>
    client-key-data: <redacted>
```
