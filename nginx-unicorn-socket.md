# What is a unix socket?

* represented by a file on disk
* data does not flow through that file
* allow communication between processes on the same host system
* two flavours
    1. stream (stream UDS or TCP)
    1. datagram (datagram UDS or UDP)
* access is controlled via file permissions
* a socket address takes the form of a pathname **but** IO on the socket does not involve any operations on the underlying filesystem
* sockets on a system can be configured via parameters under `/proc/sys/net/core/`
* a socket may be bound to only one pathname and a pathname can only be bound to one socket
    * socket pathnames should be absolute - otherwise the listener has to know your cwd to be able to figure out what path to use
* `socketpair()` used to create a pair of _connected_ unix domain sockets
* `bind()` actually creates the socket entry in the filesystem


`man 7 socket` contains description of the options

```
# $ man socket
Name                Purpose                          Man page
AF_UNIX, AF_LOCAL   Local communication              unix(7)
AF_INET             IPv4 Internet protocols          ip(7)
AF_INET6            IPv6 Internet protocols          ipv6(7)
AF_IPX              IPX - Novell protocols
AF_NETLINK          Kernel user interface device     netlink(7)
AF_X25              ITU-T X.25 / ISO-8208 protocol   x25(7)
AF_AX25             Amateur radio AX.25 protocol
AF_ATMPVC           Access to raw ATM PVCs
AF_APPLETALK        AppleTalk                        ddp(7)
AF_PACKET           Low level packet interface       packet(7)
AF_ALG              Interface to kernel crypto API

The socket has the indicated type, which specifies the communication semantics.  Currently defined types are:

SOCK_STREAM     Provides sequenced, reliable, two-way, connection-based byte streams.  An out-of-band data transmission mechanism may be supported.

SOCK_DGRAM      Supports datagrams (connectionless, unreliable messages of a fixed maximum length).

SOCK_SEQPACKET  Provides  a sequenced, reliable, two-way connection-based data transmission path for datagrams of fixed maximum length; a consumer is required to read an
                entire packet with each input system call.

SOCK_RAW        Provides raw network protocol access.

SOCK_RDM        Provides a reliable datagram layer that does not guarantee ordering.

SOCK_PACKET     Obsolete and should not be used in new programs; see packet(7).

Some socket types may not be implemented by all protocol families.

Since Linux 2.6.27, the type argument serves a second purpose: in addition to specifying a socket type, it may include the bitwise OR of any of the following values,  to
modify the behavior of socket():

SOCK_NONBLOCK   Set the O_NONBLOCK file status flag on the new open file description.  Using this flag saves extra calls to fcntl(2) to achieve the same result.

SOCK_CLOEXEC    Set  the  close-on-exec  (FD_CLOEXEC)  flag on the new file descriptor.  See the description of the O_CLOEXEC flag in open(2) for reasons why this may be
                useful.

The protocol specifies a particular protocol to be used with the socket.  Normally only a single protocol exists to support a particular socket type within a given  pro‐
tocol family, in which case protocol can be specified as 0.
```

Example code for creating a socket

```c
// "sun" here is short for "socket unix"

const char *SOCKNAME = "/tmp/mysock";
int sfd;
struct sockaddr_un addr;

// int socket(int domain, int type, int protocol);
//       SOCK_STREAM     Provides sequenced, reliable, two-way, connection-based byte streams.  An out-of-band data transmission mechanism may be supported.
//        The protocol specifies a particular protocol to be used with the socket.  Normally only a single protocol exists to support a particular socket type within a given  pro‐
//       tocol family, in which case protocol can be specified as 0.
sfd = socket(AF_UNIX, SOCK_STREAM, 0);            /* Create socket */
if (sfd == -1)
    errExit("socket");

memset(&addr, 0, sizeof(struct sockaddr_un));     /* Clear structure */
addr.sun_family = AF_UNIX;                        /* UNIX domain address */
strncpy(addr.sun_path, SOCKNAME, sizeof(addr.sun_path) - 1);


if (bind(sfd, (struct sockaddr *) &addr, sizeof(struct sockaddr_un)) == -1)
    errExit("bind");


```

## Socket docs

* you use the same functions for TCP, UDP and unix domain sockets - the socket struct is what tells the function which kind you are using

```
# overview
man 7 socket

# functions
man 2 socket
man 2 connect
man 2 bind
man 2 listen
man 2 accept
man 2 socketpair

# socket functions for sending data
man 2 send
man 2 sendto
man 2 sendmsg

# socket functions for receiving data
man 2 recv
man 2 recvfrom
man 2 recvmsg

man 2 poll
man 2 select

# standard file functions which can send data via socket
man 2 write
man 2 writev
man 2 sendfile

# standard file functions which can receive data via socket
man 2 read
man 2 readv

# close the socket fully or partially
man 2 close
man 2 shutdown

# find out info about a socket
man 2 getsockname
man 2 getpeername
man 2 getsockopt

# set options on a socket
man 2 ioctl
man 2 setsockopt



```

# Non blocking IO on a socket

```
It is possible to do nonblocking I/O on sockets by setting the O_NONBLOCK flag
on a socket file descriptor using fcntl(2).  Then all operations that would
block will (usually) return with EAGAIN (operation should be retried later);
connect(2) will return EINPROGRESS error.  The user can then wait for various
events via poll(2) or select(2)
```

# What configuration params are available on a unix socket?

See `socket(7)`

* option values are usually pointers to `int`
* options are documented in `man 7 socket` http://man7.org/linux/man-pages/man7/socket.7.html

### backlog

* the baclog is a parameter to `listen(2)`

from listen(2):

* The backlog argument defines the maximum length to which the queue of pending connections for sockfd may grow.
* If a connection request arrives when the queue is full, the client may receive an error with an indication of ECONNREFUSED or, if the underlying protocol supports retransmission, the request may be ignored so that a later reattempt at connection succeeds.
* If the backlog argument is greater than the value in `/proc/sys/net/core/somaxconn`, then it is silently truncated to that value; the default value in this file is 128.  In kernels before 2.4.25, this limit was a hard coded value, SOMAXCONN, with the value 128.

```
# on AWS t2.medium
$ cat /proc/sys/net/core/somaxconn
128
```

> Note: with the Linux kernel, the net.core.somaxconn sysctl defaults to 128, capping this value to 128. Raising the sysctl allows a larger backlog (which may not be desirable with multiple, load-balanced machines)

QUESTION: should i raise this value?

### SO_SNDBUF

Sets or gets the maximum socket send buffer in bytes.  The
kernel doubles this value (to allow space for bookkeeping
overhead) when it is set using setsockopt(2), and this doubled
value is returned by getsockopt(2).  The default value is set
by the /proc/sys/net/core/wmem_default file and the maximum
allowed value is set by the /proc/sys/net/core/wmem_max file.
The minimum (doubled) value for this option is 2048.

```
# example on AWS t2.medium (212992 bytes = 208 KB)
$ cat /proc/sys/net/core/wmem_default
212992
$ cat /proc/sys/net/core/wmem_max
212992
```

Linux assumes that half of the send/receive buffer is used for internal kernel
structures; thus the values in the corresponding /proc files are twice what can
be observed on the wire.


### SO_RCVBUF

Sets or gets the maximum socket receive buffer in bytes.  The
kernel doubles this value (to allow space for bookkeeping
overhead) when it is set using setsockopt(2), and this doubled
value is returned by getsockopt(2).  The default value is set
by the /proc/sys/net/core/rmem_default file, and the maximum
allowed value is set by the /proc/sys/net/core/rmem_max file.
The minimum (doubled) value for this option is 256.

```
# example on AWS t2.medium (212992 bytes = 208 KB)
$ cat /proc/sys/net/core/wmem_default
212992
$ cat /proc/sys/net/core/wmem_max
212992
```

Linux assumes that half of the send/receive buffer is used for internal kernel
structures; thus the values in the corresponding /proc files are twice what can
be observed on the wire.

# Performance of a unix socket vs TCP socket?

It seems that TCP sockets over loopback are slower than unix domain sockets because TCP has extra congestion control and overhead (SYN, SYN ACK etc.) but the difference only matters when the throughput is high.

Loopback TCP sockets have no special knowledge of being on the same machine. UDS sockets assume they are on the same machine.

Overhead that TCP sockets have that UDS do not:

* checksums
* flow control at IP level
* SYN, SYN ACK, RST packets etc.
* breaking data into MTU sized datagrams

References

* https://redis.io/topics/benchmarks
* https://lists.freebsd.org/pipermail/freebsd-performance/2005-February/001143.html

# Tuning Unicorn's unix domain socket

From https://bogomips.org/unicorn/TUNING.html

> If you're doing extremely simple benchmarks and getting connection errors under high request rates, increasing your :backlog parameter above the already-generous default of 1024 can help avoid connection errors. Keep in mind this is not recommended for real traffic if you have another machine to failover to (see above).
>
> For load testing/benchmarking with UNIX domain sockets, you should consider increasing net.core.somaxconn or else nginx will start failing to connect under heavy load. You may also consider setting a higher :backlog to listen on as noted earlier.

Unicorn defaults to asking for a backlog of `1024` but by default the Kernel will cap it at 128 unless you raise `maxsoconn`


```
$ sudo sysctl net.core.somaxconn
net.core.somaxconn = 128
```

