

* `/dev/pf` the packet filter device
* `pfctl` - the program that controls the packet filter device
* `sysctl` - utility for getting/setting kernel state
* `/etc/pf.conf` - packet filter device config file
* `/etc/pf.os` - passive OS fingerprint database


The packet filter does not itself forward packets between interfaces - you can enable that in via sysctl variables
