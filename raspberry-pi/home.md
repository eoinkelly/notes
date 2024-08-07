# Find the pi on the network

```
sudo nmap -sn 192.168.1.* # run as root to get network interface maker string
ssh pi@192.168.1.74
```

# Setup ethernet interface to run its own network

### Sources

* https://www.raspberrypi.org/forums/viewtopic.php?t=132674

### Instructions

Create a static network on `eth0` by editing `/etc/network/interfaces`

```
$ cat /etc/network/interfaces
# interfaces(5) file used by ifup(8) and ifdown(8)

# Please note that this file is written to be used with dhcpcd
# For static IP, consult /etc/dhcpcd.conf and 'man dhcpcd.conf'

# Include files from /etc/network/interfaces.d:
source-directory /etc/network/interfaces.d

auto lo
iface lo inet loopback

allow-hotplug wlan0
iface wlan0 inet manual
    wpa-conf /etc/wpa_supplicant/wpa_supplicant.conf

allow-hotplug wlan1
iface wlan1 inet manual
    wpa-conf /etc/wpa_supplicant/wpa_supplicant.conf

# This was the original line before I setup the static network

# 2018-08-25: Added static network to allow this Pi to control internet access
#             for AppleTV
# iface eth0 inet manual # 2018-08-25: this was the old line
iface eth0 inet static
    address 192.168.99.10
    netmask 255.255.255.0
    network 192.168.99.0
    broadcast 192.168.99.255
```

Install `dnsmasq`

````
$ sudo apt-get install dnsmasq
```

Create file `/etc/dnsmasq.conf`

```
interface=eth0 			# only listen on this interface
dhcp-range=192.168.99.11,192.168.99.20,255.255.255.0,24h
bind-interfaces      # Bind to the interface to make sure we aren't sending things elsewhere
server=1.1.1.1       # Forward DNS requests to Cloudflare DNS
domain-needed        # Don't forward short names
bogus-priv           # Never forward addresses in the non-routed address spaces.
```

Edit `/etc/sysctl.conf` and uncomment IPv4 forwarding:

```
# Uncomment the next line to enable packet forwarding for IPv4
net.ipv4.ip_forward=1
```

Setup iptables rules to allow forwarding

```
$ sudo iptables -t nat -A POSTROUTING -o wlan0 -j MASQUERADE
$ sudo sh -c "iptables-save > /etc/iptables.ipv4.nat"
$ cat /etc/iptables.ipv4.nat
# Generated by iptables-save v1.4.21 on Sat Aug 25 18:09:11 2018
*nat
:PREROUTING ACCEPT [17:2894]
:INPUT ACCEPT [17:2894]
:OUTPUT ACCEPT [23:3471]
:POSTROUTING ACCEPT [15:2835]
-A POSTROUTING -o wlan0 -j MASQUERADE
COMMIT
# Completed on Sat Aug 25 18:09:11 2018
```

Edit `/etc/rc.local` to append the line:

```
iptables-restore < /etc/iptables.ipv4.nat
```

so that the iptables changes will be persisted across reboots.

## Setup openvpn

       OpenVPN is designed to work with the TUN/TAP virtual networking interface that exists on most platforms.

```
$ apt-get install openvpn
$ sudo openvpn --config path/to/myconfig.ovpn --daemon
# this creates a new network interface tun0
```

## Forwarding eth0 to the VPN connection

THIS IS INCOMPLETE

To get `eth0` to forward to `tun0` instead of `wlan0`

```
sudo iptables -t nat -A POSTROUTING -o tun0 -j MASQUERADE
```

