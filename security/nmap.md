# # nmap -sn (basic network scan)

* does a ping scan (not a port scan)
* was `-sP` in older nmap versions
* more reliable than pinging the broadcast address as many hosts don't respond
  to that
    * TODO: find out more about pinging broadcast address

The default host discovery done with -sn consists of sending four things

1. ICMP echo request
2. TCP SYN to port 443
3. TCP ACK to port 80
4. ICMP timestamp request

by default.

* When executed by an unprivileged user, only SYN packets are sent (using a
  connect call) to ports 80 and 443 on the target.
* When a privileged user tries to scan targets on a local ethernet network, ARP
  requests are used unless --send-ip was specified.

```
nmap -sn 192.168.1.*
sudo nmap -sn 192.168.1.* # also sends ARP requests
```

```
$ nmap -sn 192.168.1.*

Starting Nmap 7.12 ( https://nmap.org ) at 2016-08-19 07:33 NZST
Nmap scan report for 192.168.1.64
Host is up (0.00024s latency).
Nmap scan report for 192.168.1.74
Host is up (0.016s latency).
Nmap scan report for 192.168.1.254
Host is up (0.046s latency).
Nmap done: 256 IP addresses (3 hosts up) scanned in 22.53 seconds


$ sudo nmap -sn 192.168.1.*
Password:

Starting Nmap 7.12 ( https://nmap.org ) at 2016-08-19 07:41 NZST
Nmap scan report for 192.168.1.69
Host is up (0.17s latency).
MAC Address: B0:65:BD:F2:65:11 (Apple)
Nmap scan report for 192.168.1.74
Host is up (0.056s latency).
MAC Address: B8:27:EB:4C:83:12 (Raspberry Pi Foundation)
Nmap scan report for 192.168.1.254
Host is up (0.013s latency).
MAC Address: B4:30:52:08:A3:45 (Huawei Technologies)
Nmap scan report for 192.168.1.64
Host is up.
Nmap done: 256 IP addresses (4 hosts up) scanned in 30.66 seconds
```
