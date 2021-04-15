# macOS Controller


Overview

* The app: `/Applications/UniFi.app`
* App data:
    * `~/Application Support/UniFi/`
    * `~/Application Support/UniFi/data` (MongoDB data)

* It bundles mongod and Java
* it stores backups as a binary .unf file
* Q: is all the data in mongo or is some on disk too?

## mongod

* runs mongodb on non-standard port

```
bin/mongod
    --dbpath /Applications/UniFi.app/Contents/Resources/data/db
    --port 27117
    --unixSocketPrefix /Applications/UniFi.app/Contents/Resources/run
    --logappend
    --logpath /Applications/UniFi.app/Contents/Resources/logs/mongod.log
    --bind_ip 127.0.0.1
```

Can use details above to conect with mongodb-shell or mongodb-compass

```
# connect with CLI
mongo mongodb://127.0.0.1:27117/

# backup the mongo database
mongodump -o ~/Desktop/unifi_mongo_backup mongodb://127.0.0.1:27117/
```

# Cheatsheet

To setup letsencrypt for the controller:
https://lg.io/2015/12/13/using-lets-encrypt-to-secure-cloud-hosted-services-like-ubiquitis-mfi-unifi-and-unifi-video.html

# Background

* The "device Authentication" credentials allow
    1. SSH access into all devices
        * you can sudo
    2. Web login for the USG at `http://<IP address of USG>`
* You can make changes via SSH but they are overwritten by the controller on next provision
* USG seems to be based on Vyatta
    * a specialized Debian-based Linux distribution with networking applications such as Quagga, OpenVPN, and many others
    * https://vyos.io/ is an OSS fork of Vyatta core
* UAP-AP seems to use busybox https://busybox.net/
    * uses dropbear ssh-server https://matt.ucc.asn.au/dropbear/dropbear.html

## SSH into an UAP-AP

```
UBNT-BZ.v3.9.27# uname -a
Linux UBNT 3.3.8 #1 Sat Mar 17 12:58:02 MDT 2018 mips GNU/Linux

UBNT-BZ.v3.9.27# pwd
/var/etc/persistent

UBNT-BZ.v3.9.27# ls -al
drwxr-xr-x    3 admin    admin            0 Jan  1  1970 .
drwxr-xr-x   16 admin    admin            0 Jun 21 11:33 ..
drwxr-xr-x    2 admin    admin            0 Jan  1  1970 cfg
-rw-------    1 admin    admin          459 Jan  1  1970 dropbear_dss_host_key
-rw-------    1 admin    admin          805 Jan  1  1970 dropbear_rsa_host_key
```

## SSH into a USG

```
$ uname -a
Linux Gateway 3.10.20-UBNT #1 SMP Fri Nov 3 15:45:37 MDT 2017 mips64 GNU/Linux
```

```
admin@Gateway:~$ ps aux
USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND
root         1  0.0  0.1   2568   760 ?        Ss   Jun03   0:45 init [2]
root         2  0.0  0.0      0     0 ?        S    Jun03   0:00 [kthreadd]
root         3  0.0  0.0      0     0 ?        S    Jun03   3:53 [ksoftirqd/0]
root         4  0.0  0.0      0     0 ?        S    Jun03   5:41 [kworker/0:0]
root         5  0.0  0.0      0     0 ?        S<   Jun03   0:00 [kworker/0:0H]
root         7  0.0  0.0      0     0 ?        S    Jun03   0:54 [migration/0]
root         8  0.0  0.0      0     0 ?        S    Jun03   0:00 [rcu_bh]
root         9  0.0  0.0      0     0 ?        S    Jun03  12:16 [rcu_sched]
root        10  0.0  0.0      0     0 ?        S    Jun03   0:17 [watchdog/0]
root        11  0.0  0.0      0     0 ?        S    Jun03   0:14 [watchdog/1]
root        12  0.0  0.0      0     0 ?        S    Jun03   0:36 [migration/1]
root        13  0.0  0.0      0     0 ?        S    Jun03   0:40 [ksoftirqd/1]
root        14  0.0  0.0      0     0 ?        S    Jun03   0:00 [kworker/1:0]
root        15  0.0  0.0      0     0 ?        S<   Jun03   0:00 [kworker/1:0H]
root        16  0.0  0.0      0     0 ?        S<   Jun03   0:00 [khelper]
root        17  0.0  0.0      0     0 ?        S<   Jun03   0:00 [netns]
root       115  0.0  0.0      0     0 ?        S<   Jun03   0:00 [writeback]
root       117  0.0  0.0      0     0 ?        S<   Jun03   0:00 [bioset]
root       119  0.0  0.0      0     0 ?        S<   Jun03   0:00 [kblockd]
root       124  0.0  0.0      0     0 ?        S<   Jun03   0:00 [ata_sff]
root       131  0.0  0.0      0     0 ?        S    Jun03   0:00 [khubd]
root       152  0.0  0.0      0     0 ?        S    Jun03   0:00 [kworker/0:1]
root       177  0.0  0.0      0     0 ?        S    Jun03   0:00 [khungtaskd]
root       178  0.0  0.0      0     0 ?        S    Jun03   0:00 [kswapd0]
root       179  0.0  0.0      0     0 ?        S    Jun03   0:00 [fsnotify_mark]
root       180  0.0  0.0      0     0 ?        S<   Jun03   0:00 [unionfs_siod]
root       181  0.0  0.0      0     0 ?        S<   Jun03   0:00 [crypto]
root       250  0.0  0.0      0     0 ?        S<   Jun03   0:00 [deferwq]
root       251  0.0  0.0      0     0 ?        S    Jun03   4:45 [kworker/1:1]
root       254  0.0  0.0      0     0 ?        S    Jun03   0:00 [scsi_eh_0]
root       255  0.0  0.0      0     0 ?        S    Jun03   0:02 [usb-storage]
root       268  0.0  0.0      0     0 ?        S<   Jun03   0:08 [kworker/0:1H]
root       269  0.0  0.0      0     0 ?        S    Jun03   0:01 [kjournald]
root       273  0.0  0.0      0     0 ?        S<   Jun03   0:00 [loop8]
root       274  0.0  0.0      0     0 ?        S<   Jun03   0:08 [kworker/1:1H]
root       349  0.0  0.0      0     0 ?        S<   Jun03   0:00 [octeon-ethernet]
root       483  0.0  0.0      0     0 ?        S    Jun03   0:00 [kjournald]
root       573  0.0  0.0   1948   276 ?        Ss   Jun03   9:27 /usr/sbin/rngd
daemon     583  0.0  0.0   2700   324 ?        Ss   Jun03   0:00 /usr/sbin/atd
root       590  0.0  0.1   2968   936 ?        Ss   Jun03   0:13 /usr/sbin/cron
root       599  0.0  0.0   2760   320 ?        Ss   Jun03   1:06 /usr/sbin/ubnt-daemon
root       600  0.0  0.5  18328  2600 ?        Sl   Jun03   1:12 /opt/vyatta/sbin/ubnt-cfgd
freerad    646  0.0  0.8  51380  4356 ?        Ssl  Jun03   0:00 /usr/sbin/freeradius
quagga     684  0.0  0.3   7616  1640 ?        Ss   Jun03   1:26 /usr/sbin/zebra -d -P 0 -i /var/run/quagga/zebra.pid -S -s 1048576
root      2004  0.0  0.0      0     0 ?        S    Jun03   0:00 [irq/117-octeon-]
root      2655  0.0  0.5   4508  2816 ?        S    Jun03   0:26 /usr/sbin/dhcpd3 -f -pf /var/run/dhcpd-unused.pid -cf /opt/vyatta/etc/dhcpd.conf -lf /var/run/dhcpd.leases
root      2769  0.1  0.1   2520   884 ?        Ss   Jun03  39:06 /usr/sbin/lldpd -H 0 -M4 -S UBNT UniFi-Gateway-3 running on v4.4.22.5086045.180508.1522 -I *,!eth0
_lldpd    2770  0.0  0.0   2520   444 ?        S    Jun03   6:06 /usr/sbin/lldpd -H 0 -M4 -S UBNT UniFi-Gateway-3 running on v4.4.22.5086045.180508.1522 -I *,!eth0
root      2855  0.0  0.0   1956   484 ?        Ss   Jun03   0:00 /sbin/netplugd -P -p /var/run/netplugd.pid
root      2909  1.3 30.8 163900 152944 ?       S    Jun03 352:23 /usr/bin/mcad
root      2912  0.0  0.4  14208  2220 ?        S    Jun03   3:56 /usr/bin/mca-monitor
root      2914  0.0  0.1   2536   780 ttyS0    Ss+  Jun03   0:00 /sbin/getty -L ttyS0 115200 vt100
root      2915  0.0  1.1  65012  5640 ?        S    Jun03   2:43 /usr/bin/linkcheck
root      2923  0.0  0.5   6452  2608 ?        S    Jun03   0:31 /usr/bin/perl /usr/bin/dpi_wlan_fw_rules.pl
root      2954  0.0  0.0   2256   468 ?        S    Jun03   0:00 /usr/sbin/telnetd -p 55523 -b 127.0.0.101 -F
www-data  2965  0.0  0.7   7768  3576 ?        S    Jun03   1:39 /usr/sbin/lighttpd -f /etc/lighttpd/lighttpd.conf
www-data  2967  0.0  0.8  18364  4132 ?        Ss   Jun03   0:00 /usr/bin/php-cgi
www-data  3012  0.0  1.1  19476  5888 ?        S    Jun03   0:01 /usr/bin/php-cgi
www-data  3013  0.0  1.1  19220  5580 ?        S    Jun03   0:01 /usr/bin/php-cgi
www-data  3014  0.0  1.1  19476  5888 ?        S    Jun03   0:00 /usr/bin/php-cgi
www-data  3015  0.0  1.1  19220  5580 ?        S    Jun03   0:00 /usr/bin/php-cgi
root      3071  0.0  1.1  15608  5536 ?        S    Jun03   7:51 /usr/bin/perl /usr/bin/perl_wrapper.pl
root      3784  0.0  0.1   3452   700 ?        Ss   Jun03   1:47 /usr/sbin/pppd call pppoe2
ntp       4122  0.0  0.4   6816  2116 ?        Ss   Jun03   6:02 /usr/sbin/ntpd -p /var/run/ntpd.pid -g -u 107:112
root      4692  0.0  0.3  18328  1940 ?        S    09:49   0:00 /opt/vyatta/sbin/ubnt-cfgd
root     13003  0.0  0.0      0     0 ?        S    06:10   0:01 [kworker/u4:2]
root     13378  0.4  1.3 129032  6464 ?        Sl   01:12   3:08 /usr/sbin/ubnt-util -f
root     13379  0.0  0.3  18328  1636 ?        S    01:12   0:00 /opt/vyatta/sbin/ubnt-cfgd
root     26925  0.0  0.3  28536  1540 ?        Sl   11:31   0:00 /usr/sbin/rsyslogd -c5
root     26978  0.0  0.0      0     0 ?        S    11:32   0:00 [kworker/u4:1]
root     27516  0.0  0.1   8028   984 ?        Ss   11:33   0:00 /usr/sbin/sshd -p 22 -o Protocol=2
root     29098  0.1  0.6  11656  3120 ?        Ss   11:40   0:00 sshd: admin [priv]
admin    29141  0.0  0.2  11656  1384 ?        R    11:40   0:00 sshd: admin@pts/0
admin    29142  0.1  0.4   4220  2128 pts/0    Ss+  11:40   0:00 -vbash
dnsmasq  29861  0.0  0.3   6000  1968 ?        S    11:43   0:00 /usr/sbin/dnsmasq -x /run/dnsmasq/dnsmasq.pid -u dnsmasq -7 /etc/dnsmasq.d,.dpkg-dist,.dpkg-old,.dpkg-new --local
admin    30342  0.0  0.2   3308  1040 pts/0    R+   11:45   0:00 ps aux
```

```
admin@Gateway:~$ df -h
Filesystem                Size      Used Available Use% Mounted on
/dev/root                 1.6G    318.8M      1.2G  20% /root.dev
unionfs                   1.6G    318.8M      1.2G  20% /
tmpfs                   241.9M    248.0K    241.7M   0% /run
tmpfs                   241.9M    248.0K    241.7M   0% /run
tmpfs                   241.9M     10.8M    231.2M   4% /var/log
tmpfs                   241.9M         0    241.9M   0% /dev/shm
tmpfs                   241.9M     32.0K    241.9M   0% /tmp
none                    241.9M      1.2M    240.8M   0% /opt/vyatta/config
/dev/sda3                 1.7G     34.7M      1.6G   2% /root.dev/ugw
unionfs                 241.9M     32.0K    241.9M   0% /opt/vyatta/config/tmp/new_config_106b6a18809a3e4bd9d8372661
```

```
admin@Gateway:~$ sudo iptables -S
-P INPUT ACCEPT
-P FORWARD ACCEPT
-P OUTPUT ACCEPT
-N AUTHORIZED_GUESTS
-N GUEST_IN
-N GUEST_LOCAL
-N GUEST_OUT
-N LAN_IN
-N LAN_LOCAL
-N LAN_OUT
-N MINIUPNPD
-N UBNT_PFOR_FW_HOOK
-N UBNT_PFOR_FW_RULES
-N UBNT_VPN_IPSEC_FW_HOOK
-N UBNT_VPN_IPSEC_FW_IN_HOOK
-N VYATTA_FW_IN_HOOK
-N VYATTA_FW_LOCAL_HOOK
-N VYATTA_FW_OUT_HOOK
-N VYATTA_POST_FW_FWD_HOOK
-N VYATTA_POST_FW_IN_HOOK
-N VYATTA_POST_FW_OUT_HOOK
-N WAN_IN
-N WAN_LOCAL
-N WAN_OUT
-A INPUT -j UBNT_VPN_IPSEC_FW_HOOK
-A INPUT -j VYATTA_FW_LOCAL_HOOK
-A INPUT -j VYATTA_POST_FW_IN_HOOK
-A FORWARD -j MINIUPNPD
-A FORWARD -j UBNT_VPN_IPSEC_FW_IN_HOOK
-A FORWARD -j UBNT_PFOR_FW_HOOK
-A FORWARD -j VYATTA_FW_IN_HOOK
-A FORWARD -j VYATTA_FW_OUT_HOOK
-A FORWARD -j VYATTA_POST_FW_FWD_HOOK
-A OUTPUT -j VYATTA_POST_FW_OUT_HOOK
-A AUTHORIZED_GUESTS -m comment --comment "AUTHORIZED_GUESTS-10000 default-action accept" -j RETURN
-A GUEST_IN -p tcp -m comment --comment GUEST_IN-3001 -m tcp --dport 53 -j RETURN
-A GUEST_IN -p udp -m comment --comment GUEST_IN-3001 -m udp --dport 53 -j RETURN
-A GUEST_IN -p tcp -m comment --comment GUEST_IN-3002 -m tcp --dport 443 -m set --match-set captive_portal_subnets dst -j RETURN
-A GUEST_IN -m comment --comment GUEST_IN-3003 -m set --match-set guest_allow_addresses dst -j RETURN
-A GUEST_IN -m comment --comment GUEST_IN-3004 -m set --match-set guest_restricted_addresses dst -j DROP
-A GUEST_IN -m comment --comment GUEST_IN-3005 -m set --match-set corporate_network dst -j DROP
-A GUEST_IN -m comment --comment GUEST_IN-3006 -m set --match-set remote_user_vpn_network dst -j DROP
-A GUEST_IN -m comment --comment GUEST_IN-3007 -m set --match-set authorized_guests dst -j DROP
-A GUEST_IN -m comment --comment "GUEST_IN-10000 default-action accept" -j RETURN
-A GUEST_LOCAL -p udp -m comment --comment GUEST_LOCAL-3001 -m udp --dport 53 -j RETURN
-A GUEST_LOCAL -p icmp -m comment --comment GUEST_LOCAL-3002 -j RETURN
-A GUEST_LOCAL -p udp -m comment --comment GUEST_LOCAL-3003 -m udp --sport 68 --dport 67 -j RETURN
-A GUEST_LOCAL -m comment --comment "GUEST_LOCAL-10000 default-action drop" -j DROP
-A GUEST_OUT -m comment --comment "GUEST_OUT-10000 default-action accept" -j RETURN
-A LAN_IN -s 192.168.1.0/24 -m comment --comment LAN_IN-6001 -j RETURN
-A LAN_IN -m comment --comment "LAN_IN-10000 default-action accept" -j RETURN
-A LAN_LOCAL -m comment --comment "LAN_LOCAL-10000 default-action accept" -j RETURN
-A LAN_OUT -d 192.168.1.0/24 -m comment --comment LAN_OUT-6001 -j RETURN
-A LAN_OUT -m comment --comment "LAN_OUT-10000 default-action accept" -j RETURN
-A VYATTA_FW_IN_HOOK -i pppoe2 -j WAN_IN
-A VYATTA_FW_IN_HOOK -i eth0.10 -j WAN_IN
-A VYATTA_FW_IN_HOOK -i eth1 -j LAN_IN
-A VYATTA_FW_LOCAL_HOOK -i pppoe2 -j WAN_LOCAL
-A VYATTA_FW_LOCAL_HOOK -i eth0.10 -j WAN_LOCAL
-A VYATTA_FW_LOCAL_HOOK -i eth1 -j LAN_LOCAL
-A VYATTA_FW_OUT_HOOK -o pppoe2 -j WAN_OUT
-A VYATTA_FW_OUT_HOOK -o eth0.10 -j WAN_OUT
-A VYATTA_FW_OUT_HOOK -o eth1 -j LAN_OUT
-A VYATTA_POST_FW_FWD_HOOK -j ACCEPT
-A VYATTA_POST_FW_IN_HOOK -j ACCEPT
-A VYATTA_POST_FW_OUT_HOOK -j ACCEPT
-A WAN_IN -m comment --comment WAN_IN-3001 -m state --state RELATED,ESTABLISHED -j RETURN
-A WAN_IN -m comment --comment WAN_IN-3002 -m state --state INVALID -j DROP
-A WAN_IN -m comment --comment "WAN_IN-10000 default-action drop" -j DROP
-A WAN_LOCAL -m comment --comment WAN_LOCAL-3001 -m state --state RELATED,ESTABLISHED -j RETURN
-A WAN_LOCAL -m comment --comment WAN_LOCAL-3002 -m state --state INVALID -j DROP
-A WAN_LOCAL -m comment --comment "WAN_LOCAL-10000 default-action drop" -j DROP
-A WAN_OUT -m comment --comment "WAN_OUT-10000 default-action accept" -j RETURN
```
