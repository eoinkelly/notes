# Basic network scan (nmap -sn)

- does a ping scan (not a port scan)
- was `-sP` in older nmap versions
- more reliable than pinging the broadcast address as many hosts don't respond
  to that
    - TODO: find out more about pinging broadcast address

The default host discovery done with -sn consists of sending four things

1. ICMP echo request
2. TCP SYN to port 443
3. TCP ACK to port 80
4. ICMP timestamp request

by default.

- When executed by an unprivileged user, only SYN packets are sent (using a
  connect call) to ports 80 and 443 on the target.
- When a privileged user tries to scan targets on a local ethernet network, ARP
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

# OS detection (nmap -O IP_ADDRESS)

- does OS fingerprinting by TCP/IP stack fingerprinting
    - sends a series of TCP and UDP packets and carefully analyzes results
        - TCP ISN
        - TCP Options support
        - TCP ordering
        - IP ID sampling
        - initial window size check
        - TCP Sequence Predictability Classification
- uses TCP timestamp option to _guess_ when machine was rebooted
    - compares results to built-in db of OS variants
- takes approx 20 sec to fingerprint a host on my home lan

```
$ nmap -O 192.168.1.254

Starting Nmap 7.12 ( https://nmap.org ) at 2016-09-03 10:42 NZST
Nmap scan report for 192.168.1.254
Host is up (0.0022s latency).
Not shown: 997 filtered ports
PORT    STATE SERVICE
23/tcp  open  telnet
80/tcp  open  http
443/tcp open  https
MAC Address: B4:30:52:08:A3:7E (Huawei Technologies)
Warning: OSScan results may be unreliable because we could not find at least 1 open and 1 closed port
Device type: general purpose
Running: Linux 2.6.X
OS CPE: cpe:/o:linux:linux_kernel:2.6
OS details: Linux 2.6.18 - 2.6.32
Network Distance: 1 hop

OS detection performed. Please report any incorrect results at https://nmap.org/submit/ .
Nmap done: 1 IP address (1 host up) scanned in 20.12 seconds
```
