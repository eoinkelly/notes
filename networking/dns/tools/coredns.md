# CoreDNS

https://coredns.io/

- A DNS server, an alternative to BIND etc.
- almost all functionality outsourced to plugins
- it operates a bit like Rack but for DNS requests not HTTP requests
- plugins can
    - generate a response themselves
    - generate no response but have some side effect e.g. metrics or cache
    - communicate with kubernetes to provide service discovery
    - read from a file or database
- 30 plugins in default install, many more available
- performs
    - DNS
    - Service Discovery
- written in go
- chains plugins
- each plugin performs a DNS function e.g.
    - k8s service discovery
    - prometheus metrics
    - rewriting queries
    - serving zone files (old-school dns behaviour)
- integrates with k8s via k8s plugin
- integrates with etcd via plugin
- has plugins for major cloud dns services too e.g. route53
- configuring is in `Corefile`
- tries to have sane defaults

```
❯ coredns -plugins
Server types:
  dns

Caddyfile loaders:
  flag
  default

Other plugins:
  dns.acl
  dns.any
  dns.auto
  dns.autopath
  dns.azure
  dns.bind
  dns.bufsize
  dns.cache
  dns.cancel
  dns.chaos
  dns.clouddns
  dns.debug
  dns.dns64
  dns.dnssec
  dns.dnstap
  dns.erratic
  dns.errors
  dns.etcd
  dns.file
  dns.forward
  dns.geoip
  dns.grpc
  dns.header
  dns.health
  dns.hosts
  dns.k8s_external
  dns.kubernetes
  dns.loadbalance
  dns.local
  dns.log
  dns.loop
  dns.metadata
  dns.minimal
  dns.nsid
  dns.pprof
  dns.prometheus
  dns.ready
  dns.reload
  dns.rewrite
  dns.root
  dns.route53
  dns.secondary
  dns.sign
  dns.template
  dns.tls
  dns.trace
  dns.transfer
  dns.whoami
  on
```

Corefile

- defines 1+ server blocks which are groups of settings for a particular zone
- each server block
    - can specify which port it is listening on
    - you can have multiple server blocks listening on same port
    - you can bind to multiple ports
    - defines a chain of plugins to run the request through
- supports DNS, DNS over TLS, DNS over HTTP2 DNS over gRPC
