Installing docker for mac

* installes `kubectl`

Enabling kubernetes in docker for mac does the following:

* Starts 18 containers
* Causes docker for mac to use approx 4.15 GB memory on my laptop ...


    IMAGE COMMAND

1. `docker/kube-compose-controller  "/compose-controller --kubeconfig --reconciliation-interval 30s"`
2. `k8s.gcr.io/kube-proxy-amd64    "/usr/local/bin/kube-proxy --config=/var/lib/kube-proxy/config.conf"`
3. `docker/kube-compose-api-server  "/api-server --kubeconfig --authentication-kubeconfig --authorization-kubeconfig --etcd-servers=https://127.0.0.1:2379 --etcd-cafile=/etc/docker-compose/etcd/ca.crt --etcd-certfile=/etc/docker-compose/etcd/client.crt --etcd-keyfile=/etc/docker-compose/etcd/client.key --secure-port=9443 --tls-cert-file=/etc/docker-compose/tls/server.crt --tls-private-key-file=/etc/docker-compose/tls/server.key"`
4. `k8s.gcr.io/pause-amd64:3.1   "/pause"`
5. `k8s.gcr.io/pause-amd64:3.1   "/pause"`
6. `k8s.gcr.io/pause-amd64:3.1   "/pause"`
7. `k8s.gcr.io/kube-scheduler-amd64   "kube-scheduler --address=127.0.0.1 --leader-elect=true --kubeconfig=/etc/kubernetes/scheduler.conf"`
8. `k8s.gcr.io/pause-amd64:3.1   "/pause"`
9. `k8s.gcr.io/kube-controller-manager-amd64 "kube-controller-manager --address=127.0.0.1 --leader-elect=true --controllers=*,bootstrapsigner,tokencleaner --kubeconfig=/etc/kubernetes/controller-manager.conf --root-ca-file=/run/config/pki/ca.crt --cluster-signing-cert-file=/run/config/pki/ca.crt --cluster-signing-key-file=/run/config/pki/ca.key --use-service-account-credentials=true --service-account-private-key-file=/run/config/pki/sa.key"`
10. `k8s.gcr.io/pause-amd64:3.1   "/pause"`
11. `k8s.gcr.io/kube-apiserver-amd64   "kube-apiserver --admission-control=Initializers,NamespaceLifecycle,LimitRanger,ServiceAccount,DefaultStorageClass,DefaultTolerationSeconds,NodeRestriction,MutatingAdmissionWebhook,ValidatingAdmissionWebhook,ResourceQuota --requestheader-group-headers=X-Remote-Group --requestheader-extra-headers-prefix=X-Remote-Extra- --requestheader-allowed-names=front-proxy-client --kubelet-client-certificate=/run/config/pki/apiserver-kubelet-client.crt --proxy-client-cert-file=/run/config/pki/front-proxy-client.crt --insecure-port=0 --enable-bootstrap-token-auth=true --proxy-client-key-file=/run/config/pki/front-proxy-client.key --allow-privileged=true --requestheader-username-headers=X-Remote-User --secure-port=6443 --client-ca-file=/run/config/pki/ca.crt --tls-private-key-file=/run/config/pki/apiserver.key --requestheader-client-ca-file=/run/config/pki/front-proxy-ca.crt --kubelet-preferred-address-types=InternalIP,ExternalIP,Hostname --service-cluster-ip-range=10.96.0.0/12 --service-account-key-file=/run/config/pki/sa.pub --advertise-address=192.168.65.3 --tls-cert-file=/run/config/pki/apiserver.crt --kubelet-client-key=/run/config/pki/apiserver-kubelet-client.key --authorization-mode=Node,RBAC --etcd-servers=https://127.0.0.1:2379 --etcd-cafile=/run/config/pki/etcd/ca.crt --etcd-certfile=/run/config/pki/apiserver-etcd-client.crt --etcd-keyfile=/run/config/pki/apiserver-etcd-client.key"`
12. `k8s.gcr.io/pause-amd64:3.1   "/pause"`
13. `sha256:6f7f2dc7fab5d7e7f99dc4ac176683a981a9ff911d643b9f29ffa146838deda3 "/sidecar --v=2 --logtostderr --probe=kubedns,127.0.0.1:10053,kubernetes.default.svc.cluster.local,5,SRV --probe=dnsmasq,127.0.0.1:53,kubernetes.default.svc.cluster.local,5,SRV"`
14. `sha256:c2ce1ffb51ed60c54057f53b8756231f5b4b792ce04113c6755339a1beb25943 "/dnsmasq-nanny -v=2 -logtostderr -configDir=/etc/k8s/dns/dnsmasq-nanny -restartDnsmasq=true -- -k --cache-size=1000 --no-negcache --log-facility=- --server=/cluster.local/127.0.0.1#10053 --server=/in-addr.arpa/127.0.0.1#10053 --server=/ip6.arpa/127.0.0.1#10053"`
15. `sha256:80cc5ea4b547abe174d7550b82825ace40769e977cde90495df3427b3a0f4e75 "/kube-dns --domain=cluster.local. --dns-port=10053 --config-dir=/kube-dns-config --v=2"`
16. `k8s.gcr.io/pause-amd64:3.1  "/pause"`
17. `sha256:52920ad46f5bf730d0e35e11215ec12d04ca5f32835f2ceb8093d0bd38930735 "etcd --listen-client-urls=https://127.0.0.1:2379 --trusted-ca-file=/run/config/pki/etcd/ca.crt --peer-cert-file=/run/config/pki/etcd/peer.crt --peer-trusted-ca-file=/run/config/pki/etcd/ca.crt --advertise-client-urls=https://127.0.0.1:2379 --client-cert-auth=true --peer-client-cert-auth=true --data-dir=/var/lib/etcd --cert-file=/run/config/pki/etcd/server.crt --key-file=/run/config/pki/etcd/server.key --peer-key-file=/run/config/pki/etcd/peer.key"`
18. `k8s.gcr.io/pause-amd64:3.1  "/pause"`


It creates

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

