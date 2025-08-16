# SSH

TODO: dig into how you can tunnel ssh within DNS, HTTP or raw ICMP traffic

## Basics

- SSH server has a key-pair
- SSH client has a key-pair
- When the connection is setup the server and client negotiate a single-use
  keypair for use in just this connection and then use that to negotiate a
  shared symmetric key
    - The symmetric key is used because it is more efficent for
      encrypting/decrypting large amounts of data
    - The assymetric key pairs are used because they avoid having to communicate
      and store a shared secret for use in all connections
- You can used `Match` statements in `sshd_config` and `ssh_config` to limit
  configuration to certain users, hostnames, IP addresses, networks
    - `Match` statements should be at the end of the file

Aside: On macOS the SSH server is enabled by enabling remote login in System
Preferences.

## ~/.ssh

- file perms on ~/.ssh should be 0600

```
# ssh1 key pair:
.ssh/identity
.ssh/identity.pub

# ssh2 key pair:
.ssh/id_rsa
.ssh/id_rsa.pub
```

## ~/.ssh/config

A SSH config that will stop me getting booted off servers after (too!) short
periods of inactivity

```bash
# ~/.ssh/config
Host *
  # total inactive grace period =  count_max * interval

  # ServerAliveCounMax is the number of times client will send 'are you alive?'
  # message without hearing a response (default is 3)
  ServerAliveCountMax 20

  # ServerAliveInterval is how often to send the 'are you alive?' message if
  # there has been no activity. (default is 0 aka do not send)
  ServerAliveInterval 60
```

## SSH Keys

From `ssh-keygen` man page

> ssh-keygen can create RSA keys for use by SSH protocol version 1 and DSA,
> ECDSA or RSA keys for use by SSH protocol version 2. The type of key to be
> generated is specified with the -t option. If invoked without any arguments,
> ssh-keygen will generate an RSA key for use in SSH protocol 2 connections.

> 2048 bit keys are made by default

Naming your key `id_rsa` is useful because `ssh` looks for it by default. You
have to specify name if you use a different name.

From stackoverflow
http://security.stackexchange.com/questions/45193/in-what-ways-does-increasing-ssh-host-key-length-increase-security

> 768 bit RSA key was successfully bruteforced in 2010 and it was predicted that
> with computational advances 1024 bit will become weak in upcoming decade.
> Hence, it is advisable to increase key strength to 2048 in near future.

ssh-keygen supports 3 formats for import/export:

1. RFC4716 (RFC 4716/SSH2 public or private key)
2. PKCS8 (PEM PKCS8 public key)
3. PEM (PEM public key)

The default conversion format is `RFC4716`.

## Commands for working with SSH keys

```bash
# Make a new SSH key (-C is the comment)
# 2048 bits is default, using 4096 for extra protection against brute forcing the key
ssh-keygen -t rsa -b 4096 -C 'Firstname Lastname me@my_email_address.com'
# (you will be prompted for new private key password)

# Check password of existing key
openssl rsa -text -noout -in id_rsa

# Change password of existing SSH key
ssh-keygen -p -f keyfile

# Add a private key to the agent (and create an entry in OSX Keychain formatted
# in such a way to allow 'ssh-add -A' to find the password in future)
ssh-add -K ./path/to/privatekey
# (you will be prompted for private key password)

# Show fingerprints (key-length, shasum, key-type) of identities currently loaded into agent
ssh-add -l

# Show full public keys of identities currently loaded into agent (not as useful as -l)
ssh-add -L

# delete all identities from the agent
ssh-add -D

# Search the macOS keychain for identities and add them to the agent (note this
# is a macOS specific thing). You will be prompted for passwords for any private
# keys which require them.
ssh-add -A

# quick check of server health
ssh deploy@example.com 'hostname; uptime'
```

## On a mac

- On any platform `ssh-agent` needs to be started when a user logs in
- macOS does this for you - launchd will start it and re-start it if it is
  killed
    - => you can't easily kill `ssh-agent` on macOS
- `ssh-add -A` will find the key provided the password appears as an
  "application password" with the "account" field set to the path of to the
  private key and the title prefixed by "SSH:"
- Apple has patched `ssh-agent` and `ssh-add` to support OSX Keychain
- Launchd creates a unix domain socket and stores path to it in SSH_AUTH_SOCK
    - other process owned by the current user (TODO: check this) can communicate
      with the agent via this
    - TO CHECK: osx creates the unix socket but only starts ssh-agent the first
      time somebody accesses it

- TODO: figure out how to remove keys from it after a timeout

## Port forwarding

Content from this taken from a few different SSH books

> you can put any TCP/IP traffic inside SSH

- You can create a SocksV5 proxy with SSH.
- SocksV5 supports TCP or UDP over IPv4 or IPv6.
- I don't think you can send arbitrary IP packets over SocksV5

Some applications misbehave when their network connection is through an SSH
tunnel:

- use `netcat` or similar to troubleshoot the tunnel separate to the application
- sometimes adding a `hosts` file entry will make the app happier

Three kinds of port forwarding

1. Local ("listen on local side")
    - common uses
        1. to wrap an unencrypted service in encryption e.g. POP or SMTP
    - a port on the client is forwarded to a port on the server
    - the "listening" happens on the local/client side
    - causes the ssh-client to start listening on a local port. when a
      connection is made to that local port the ssh-client will forward the
      traffic through the ssh-tunnel to and the ssh-server will then make a
      connection to the given remote-port and send the traffic to that port on
      behalf of the ssh-client
1. Remote ("listen on remote side")
    - common uses
        1. to access a service behind a firewall
    - a port on the server is forwarded to a port on the client.
    - the "listening" happens on the remote/server side
    - this reversal of the role of client and server is whey this is sometimes
      called a "reverse tunnel"
    - typically when an SSH client connects to an SSH server, it is the client
      which is issuing commands to run on the server. With a remote tunnel you
      can allow the server to create a shell on the client and send it commands
    - causes the ssh-server to start listening on a given port on its side. when
      anything makes a connection to that port, the ssh-server will forward that
      traffic to the ssh-client. The ssh-client will then forward that data to a
      given local port on its side
    - example you can configure your ssh-server to listen on 127.0.0.1:2222 on
      its side and any connection made to that will be forwarded to 127.0.0.1:22
      on the client side.
        - it will seem to the ssh-server on the client side that somebody from
          localhost is connecting to it.
1. Dynamic
    - creates a SOCKSv5 proxy on the ssh-client side
    - tunnels any requests to that proxy to the ssh-server. The server then
      completes those requests as its own access permits and send replies back
      through the tunnel
    - kind of creates a proxy with two sides, one on either side of the tunnel
    - the ssh-client can now access anything the ssh-server can access
    - socks clients
        - most web browsers can be socks clients
        - TODO: need to tell the client to forward all its DNS requests through
          the proxy

### Local (listen on the local/client side) forwarding

```bash
# 127.0.0.1 and localhost are synonyms

ssh username@hostname # normal SSH session

# setup local forwarding
# the pattern is ssh -L {from}:{to} ...
ssh -L localIP:localPort:remoteIP:remotePort username@hostname
# localIP defaults to 127.0.0.1 so can also use
ssh -L localPort:remoteIP:remotePort username@hostname

# listen on 127.0.0.1:2222 on the client side and forward all traffic on that port to 127.0.0.1:80 on the server side
ssh -L 127.0.0.1:2222:127.0.0.1:80 username@hostname
# same as above
ssh -L 2222:127.0.0.1:80 username@hostname
```

```bash
# in ssh_config file
Host something.example.com
    LocalForward localhost:2222 localhost:80

    # if you want to bind to any interface other than 'localhost' on the client
    # side then you also need the following:

    GatewayPorts yes # values=yes|no, default=no
    # yes = allow ssh client to request to listen on any port (note the subtle difference in meaning between here and sshd_config)
    # no = prevent server from binding to (listening on) any interface other than 127.0.0.1
    LocalForward 192.168.1.40:2222 localhost:80
```

### Remote (listen on the remote/server side) forwarding

If you want an ssh-server process to listen on an interface that isn't localhost
then you need to explicitly enable that in `sshd_config`

```bash
# in sshd_config (the server configuration)

# if you want to bind to any interface other than 'localhost' on the server
# side then you need the following

GatewayPorts yes # values=yes|no|clientspecified, default=no
# yes = automatically bind to all interfaces every time!
# clientspecified = let clients bind to whatever they request (don't auto bind to every interface like 'yes' does)
# no = prevent server from binding to (listening on) any interface other than 127.0.0.1

# remember that you will need to be root to bind to anything in 0-1024 range on
# unix
```

```bash
ssh username@hostname # normal SSH session

# setup remote forwarding
# the pattern is ssh -L {from}:{to} ...
ssh -R remoteIP:remotePort:localIP:localPort username@hostname
# remoteIP defaults to 127.0.0.1 so can also use
ssh -R remotePort:localIP:localPort username@hostname

# Anyone who connects to loalhost:2222 on the server will have their traffic
# forwarded to port 22 on the client
ssh -R 127.0.0.1:2222:127.0.0.1:22 username@hostname
# same as above
ssh -R 2222:127.0.0.1:22 username@hostname
ssh -R localhost:2222:localhost:22 username@hostname
ssh -R 2222:localhost:22 username@hostname
```

```bash
# in ssh_config file
Host something.example.com
    RemoteForward localhost:2222 localhost:22
```

### Dynamic (setup SOCKSv5 proxy on client) forwarding

- You might want to have the local Socksv5 proxy listen on something other than
  `localhost` if you want to turn your machine into a proxy that others on your
  network can use.

```bash
# 127.0.0.1 and localhost are synonyms

ssh username@hostname # normal SSH session

# setup dynamic forwarding
ssh -D localIp:localPort username@hostname

# localIP defaults to 127.0.0.1 so can also use
ssh -D localPort username@hostname
```

```bash
# in ssh_config file
Host something.example.com
    # DynamicForward localIp:localPost
    # DynamicForward 127.0.0.1:9999
    DynamicForward 9999

    # if you want to bind to any interface other than 'localhost' on the server
    # side then you need the following:

    GatewayPorts yes # values=yes|no, default=no
    # yes = allow ssh client to request to listen on any port (note the subtle difference in meaning between here and sshd_config)
    # no = prevent server from binding to (listening on) any interface other than 127.0.0.1
```

#### Use-case: browsing a website hosted by the server from the server

1. Setup dynamic forwarding as above
1. In firefox, go to `about:preferences#general` (a special URL) and open the
   connection settings
1. Choose to use a SocksV5 proxy with hostname `127.0.0.1` and port of whatever
   you picked for your port in step 1.
1. Tick the box for "Proxy DNS when using SocksV5"
1. Remove the rules which skip the proxy for `127.0.0.1,localhost`
1. Save settings
1. Browse to `http://localhost/` in Firefox - you are now seeing the site as you
   would if you ran `curl http://localhost/` on the server.

### Useful flags to go with forwarding of all kinds

```
-f  put the ssh client into the background
-N  tell SSH not to start a terminal session on the server
```

### Controlling what port forwarding is allowed via server config

Note that you cannot totally prevent port forwarding if your users have shell
access and can run any binary then they can make it work. You can at least make
it less easy.

```bash
# /etc/ssh/sshd_config

AllowTcpForwarding yes # values=yes|no|local|remote, default=yes

# yes = automatically bind to all interfaces every time!
# clientspecified = let clients bind to whatever they request (don't auto bind to every interface like 'yes' does)
# no = prevent server from binding to (listening on) any interface other than 127.0.0.1
GatewayPorts no # values=yes|no|clientspecified, default=no

# List the interfaces and ports which are explicitly allowed to be bound (all others are prevented)
PermitOpen localhost:25 localhost:10
```

### Controlling what port forwarding is allowed via client config

The client can control which of its ports can be bound to during local port
forwarding.

```bash
# ssh_config

GatewayPorts no # values=yes|no, default=no
# yes = allow ssh client to request to listen on any port (note the subtle difference in meaning between here and sshd_config)
# no = prevent server from binding to (listening on) any interface other than 127.0.0.1

# tell SSH client whether to exit if the forwarding fails (useful if forwarding
# is the whole reason for the SSH session)
ExitOnForwardFailure no # values=yes|no, default=no
```

## Keeping SSH sessions alive

There are 3 ways of keeping an SSH connection alive

1. Run something like `top` which will keep data folowing
    - ++ doesn't require you to change any settings
    - -- you have to remember to do it every time
1. Configure TCP keepalives
    - not as configurable as SSH keep-alives but sufficent for many uses
    - not part of the SSH protocol - TCP keepalives are features of the client
      and the server
    - just involves sending some ping/pong packets on a regular basis
    - OpenSSH sends TCP keepalives by default - can be turned off with
      `TcpKeepAlive no`
    - Client and server (or both) can send keepalive TCP packets
    - TCP keepalives are **not** sent in the encrypted channel - they are
      visible to anybody on the network who can see the traffic and so they can
      be spoofed.
1. Configure "SSH keepalives"
    - Sent within the encrypted channel
    - More information rich - TCP keepalive just tells the other end that the
      TCP connection is still open, SSH keepalive tells the other end that the
      SSH session is still viable.
    - Clients send "client alive" messages
    - Servers send "server alive" messages
    - OpenSSH does not use SSH keepalives by default
    - Putty does not support SSH keepalives

Note that TCP connection timeouts ultimately control the SSH session - if you
cannot maintain a TCP session between client and server then nothing which
happens at the SSH layer will matter.

For managing an SSH server you should at least have one of TCP keepalives or SSH
keepalives - without one of them the SSH server will have no way of knowing when
a client goes away so it will keep running an SSH session for that connection.
Ultimately this will cause you to run out of resources.

From the server point of view we want to kick off dead clients in a timely
manner to preserve resources - some of this is taken care of by the TCP
connection anyway which is presumably why SSh keepalives default to disabled on
the server side.

The fact taht TCPKeepAlive is enabled on the server by default means that the
server will disconnect if anything happens to the TCP session - this makes SSH
brittle to flaky networks - use Mosh if you have to maintain a shell over a very
flaky connection.

```bash
# sshd_config

# After how many seconds of inactivity should we check that the client is still
# alive?
ClientAliveInterval 90 # value=seconds, default=0 (disabled)

# How many of our "are you alive?" checks can go unanswered before we consider
# the client dead and close the connection?
ClientAliveCountMax 5 # value= , default=3
```

```bash
# ssh_config

ServerAliveInterval
ServerAliveCountMax
```

```bash
# From the ssh_config man page
ServerAliveCountMax
        Sets the number of server alive messages (see below) which may be
        sent without ssh(1) receiving any messages back from the server.
        If this threshold is reached while server alive messages are being
        sent, ssh will disconnect from the server, terminating the ses-
        sion.  It is important to note that the use of server alive mes-
        sages is very different from TCPKeepAlive (below).  The server
        alive messages are sent through the encrypted channel and there-
        fore will not be spoofable.  The TCP keepalive option enabled by
        TCPKeepAlive is spoofable.  The server alive mechanism is valuable
        when the client or server depend on knowing when a connection has
        become inactive.

        The default value is 3.  If, for example, ServerAliveInterval (see
        below) is set to 15 and ServerAliveCountMax is left at the
        default, if the server becomes unresponsive, ssh will disconnect
        after approximately 45 seconds.

ServerAliveInterval
        Sets a timeout interval in seconds after which if no data has been
        received from the server, ssh(1) will send a message through the
        encrypted channel to request a response from the server.  The
        default is 0, indicating that these messages will not be sent to
        the server.

TCPKeepAlive
        Specifies whether the system should send TCP keepalive messages to
        the other side.  If they are sent, death of the connection or
        crash of one of the machines will be properly noticed.  However,
        this means that connections will die if the route is down tempo-
        rarily, and some people find it annoying.

        The default is yes (to send TCP keepalive messages), and the
        client will notice if the network goes down or the remote host
        dies.  This is important in scripts, and many users want it too.

        To disable TCP keepalive messages, the value should be set to no.
```

```bash
# From the sshd_config man page
ClientAliveCountMax
        Sets the number of client alive messages which may be sent without
        sshd(8) receiving any messages back from the client.  If this
        threshold is reached while client alive messages are being sent,
        sshd will disconnect the client, terminating the session.  It is
        important to note that the use of client alive messages is very
        different from TCPKeepAlive.  The client alive messages are sent
        through the encrypted channel and therefore will not be spoofable.
        The TCP keepalive option enabled by TCPKeepAlive is spoofable.
        The client alive mechanism is valuable when the client or server
        depend on knowing when a connection has become inactive.

        The default value is 3.  If ClientAliveInterval is set to 15, and
        ClientAliveCountMax is left at the default, unresponsive SSH
        clients will be disconnected after approximately 45 seconds.

ClientAliveInterval
        Sets a timeout interval in seconds after which if no data has been
        received from the client, sshd(8) will send a message through the
        encrypted channel to request a response from the client.  The
        default is 0, indicating that these messages will not be sent to
        the client.

TCPKeepAlive
        Specifies whether the system should send TCP keepalive messages to
        the other side.  If they are sent, death of the connection or
        crash of one of the machines will be properly noticed.  However,
        this means that connections will die if the route is down tempo-
        rarily, and some people find it annoying.  On the other hand, if
        TCP keepalives are not sent, sessions may hang indefinitely on the
        server, leaving "ghost" users and consuming server resources.

        The default is yes (to send TCP keepalive messages), and the
        server will notice if the network goes down or the client host
        crashes.  This avoids infinitely hanging sessions.

        To disable TCP keepalive messages, the value should be set to no.
```

## Managing SSH in the middle of a terminal session

- The escape key is `~`

```
$ ~?
Supported escape sequences:
 ~.   - terminate connection (and any multiplexed sessions)
 ~B   - send a BREAK to the remote system
 ~C   - open a command line
 ~R   - request rekey
 ~V/v - decrease/increase verbosity (LogLevel)
 ~^Z  - suspend ssh
 ~#   - list forwarded connections
 ~&   - background ssh (when waiting for connections to terminate)
 ~?   - this message
 ~~   - send the escape character by typing it twice
(Note that escapes are only recognized immediately after newline.)

$ ~. # exit the session (handy if the session is hung)

$ ~C # edit forwarding settings (enters the ssh> prompt

ssh>-D9999 # setup dynamic port forwarding on localhost:9999
ssh>-KD9999 # cancel the previous port forwarding
```

## SSH as VPN

- SSH can function as a VPN
- OpenBSD has the best support (PuTTY has none)
- SSH can function as a VPN but isn't a great choice. TCP packets tunneled
  inside TCP packets mucks with the congestion and retransmission algorithms
  built-in to TCP and amplifies the effects of any packet loss making them
  unreliable.
    - TL;DR - prefer OpenVPN over SSH for a VPN
- it works by
    - creating a "tunnel interface" (aka a "tun interface")
    - the tun interface sits _above_ some other interface
    - the tun interface gets a device number like other interfaces e.g. `tun0`
- point-to-point tunnles are your best option
- you can tunnel ethernet traffic over SSH but you should avoid it if you can
    - SSH tunnels are already suseptible to congestion, sending layer2 traffic
      over them makes that even worse
- Using an SSH VPN requires root privileges on both the client **and** the
  server (you need to create network devices)
- You run the SSH server as root **and** you must login as root

```bash
# sshd_config

PermitTunnel no # values=yes|no|point-to-point|ethernet, default=no
```
