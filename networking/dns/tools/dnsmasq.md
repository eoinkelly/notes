# dnsmasq

* https://thekelleys.org.uk/dnsmasq/doc.html
* designed to provide network infrastructure to small networks
* a forwarder not a recursive resolver
* single threaded C
* will cache answers it receives
* supports dnssec
* will load /etc/hosts
    * Adding `0.0.0.0 adssite.com` to hosts files lets it function as basic adblocker
* not just dns: proviced DHCP, network boot features, router advertisement
* pi-hole and Unifi routers use dnsmasq i.e. they are **forwarders** not **recursive resolvers**
    * you can run unbound locally and point your pi-hole at it
