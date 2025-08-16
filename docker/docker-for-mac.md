# Docker for mac

- runs as a native Mac application
- uses xhyve to virtualize the Docker Engine environment and Linux
  kernel-specific features for the Docker daemon.

```
# To get a session on the docker host use:
screen ~/Library/Containers/com.docker.docker/Data/com.docker.driver.amd64-linux/tty

# then Ctrl-A Ctrl-\ to exit (typing exit does not work)
```

Xhyve

https://github.com/mist64/xhyve/

> The xhyve hypervisor is a port of bhyve to OS X. It is built on top of
> Hypervisor.framework in OS X 10.10 Yosemite and higher, runs entirely in
> userspace, and has no other dependencies. It can run FreeBSD and vanilla Linux
> distributions and may gain support for other guest operating systems in the
> future.

> bhyve is the FreeBSD hypervisor, roughly analogous to KVM + QEMU on Linux. It
> has a focus on simplicity and being legacy free.

- At installation time, Docker for Mac provisions an HyperKit VM based on Alpine
  Linux, running Docker Engine.
- It exposes the docker API on a socket in /var/tmp/docker.sock.
- Since this is the default location where docker will look if no environment
  variables are set, you can start using docker and docker-compose without
  setting any environment variables

You can't route network traffic between containers and the host when you are
running "docker for mac" QUESTION: which "host"? the linux-vm or macOS ???
