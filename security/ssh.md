# SSH

## ~/.ssh

* file perms on ~/.ssh should be 0600

```
# ssh1 key pair:
.ssh/identity
.ssh/identity.pub

# ssh2 key pair:
.ssh/id_rsa
.ssh/id_rsa.pub
```

## ~/.ssh/config

A SSH config that will stop me getting booted off servers after (too!) short periods of inactivity

```
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
> ECDSA or RSA keys for use by SSH protocol version 2.  The type of key to be
> generated is specified with the -t option.  If invoked without any arguments,
> ssh-keygen will generate an RSA key for use in SSH protocol 2 connections.

> 2048 bit keys are made by default

Naming your key `id_rsa` is useful because `ssh` looks for it by default. You
have to specify name if you use a different name.

From stackoverflow http://security.stackexchange.com/questions/45193/in-what-ways-does-increasing-ssh-host-key-length-increase-security

> 768 bit RSA key was successfully bruteforced in 2010 and it was predicted
> that with computational advances 1024 bit will become weak in upcoming
> decade. Hence, it is advisable to increase key strength to 2048 in near
> future.


ssh-keygen supports 3 formats for import/export:

1. RFC4716 (RFC 4716/SSH2 public or private key)
2. PKCS8   (PEM PKCS8 public key)
3. PEM     (PEM public key)

The default conversion format is `RFC4716`.

## Commands for working with SSH keys

```
# Make a new SSH key (-C is the comment)
ssh-keygen -t rsa -b 4096 -C 'Firstname Lastname me@my_email_address.com'

# Check password of existing key
openssl rsa -text -noout -in id_rsa

# Change password of existing SSH key
ssh-keygen -p -f keyfile

# Show which keys are loaded into agent
ssh-add -l

# quick check of server health
ssh deploy@servers.com 'hostname; uptime'
```

