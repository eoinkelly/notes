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
# 2048 bits is default, using 4096 for extra protection against brute forcing the key
ssh-keygen -t rsa -b 4096 -C 'Firstname Lastname me@my_email_address.com'
# (you will be prompted for new private key password)

# Check password of existing key
openssl rsa -text -noout -in id_rsa

# Change password of existing SSH key
ssh-keygen -p -f keyfile

# Add a private key to the agent (and store creds in OSX Keychain)
ssh-add -K ./path/to/privatekey
# (you will be prompted for private key password)

# Show which keys are loaded into agent
ssh-add -l

# quick check of server health
ssh deploy@servers.com 'hostname; uptime'
```

## On a mac

* On any platform `ssh-agent` needs to be started when a user logs in
* macOS does this for you - launchd will start it and re-start it if it is killed
    * => you can't easily kill `ssh-agent` on macOS
* launchd will load any keys mentioned in your keychain
    * in keychain the password appears as an "application password" with the
      "account" field set to the path of to the private key and the title
      prefixed by "SSH:"
* Apple has patched `ssh-agent` and `ssh-add` to support OSX Keychain
    * ++ you don't have to explicitly call `ssh-add` at the start of a session to add your keys
    * -- the ssh-agent will keep the keys there for a long as you are logged in
* Launchd creates a unix domain socket and stores path to it in SSH_AUTH_SOCK
    * other process owned by the current user (TODO: check this) can communicate with the agent via this
    * TO CHECK: osx creates the unix socket but only starts ssh-agent the first time somebody accesses it

* TODO: figure out how to remove keys from it after a timeout

```
http://www-uxsup.csx.cam.ac.uk/~aia21/osx/leopard-ssh.html

Mac OS X Leopard modifies SSH agent so that it is started via the Mac OS X
launchd service on demand (i.e. it will be launched on first use).

Going even further, Mac OS X Leopard modifies the SSH tools to support storing
the pass phrases in the user's Keychain. This means that if the user chooses to
store their pass phrase(s) in the Keychain they never need to enter their pass
phrase again once they have added it to their Keychain.

To store the passphrase for your default key in the Keychain open a Terminal
and run:

ssh-add -K

And to store the passphrase for a different key run:

ssh-add -K /path/to/private/key/file
```
