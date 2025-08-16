# systemd

Resources

- http://jtimberman.housepub.org/blog/2012/12/29/process-supervision-solved-problem/
- docs: https://wiki.ubuntu.com/SystemdForUpstartUsers
- https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/7/html/System_Administrators_Guide/chap-Managing_Services_with_systemd.html

Basics

- replaces the sysv inti system
- contains
    - daemons
    - libraries _ utilities _ works only with linux (not unix)
- most distros use it now
- starts units in a lazy, dependency-based way - this is quite different to
  upstart works (upstart was event based)
- systemd manages "units"
- units
    - units are named `name.type`
    - 12 types of unit available
        1. `.service`
            - a process or group of processes controlled by systemd
        2. `.swap`
        3. `.socket`
            - you tell systemd to listen on a socket (unix, tcp, udp) and tell
              it what service to start when a connection comes in
            - I **think** this is an alternative to having a server listen on a
              socket, systemd starts the service just in time and then the
              service shuts down afterwards
        4. `.device`
        5. `.mount`
        6. `.automount`
        7. `.target`
        8. `.timer`
            - cron functionality
            - you define a `foo.timer` which is a bit like a crontab line and
              you point it at a `foo.service` to run when the timer triggers
        9. `.path`
        10. `.snapshot`
        11. `.slice`
        - a type of unit used to create a cgroup heirarchy to help us do
          resource management
        12. `.scope`
            - allows you to group a service's worker processes
    - a unit is defined by a plain text file in a sort of INI format
        - file can have following headings
            - `[Unit]`
            - `[Service]`
            - `[Install]`
            - others
- allows you to assign "shares" for CPU, memory, IO to split up how you want to
  allocate resources
    - see output of `systemd-cgls`
- locations
    - `/usr/lib/systemd/system`
        - DO NOT EDIT files in here - debs/rpms put files here and reinstalls
          will overwrite your tweaks[jj
    - `/etc/systemd/system`
        - you should edit files here
        - takes precedence over `/usr/lib/systemd/system`
    - `/run/systemd/system`
        - non persistent, runtime changes
    - users can have their own location
- systemd creates a kernel cgroup for every service it runs
    - allows you to set resource limits
    - allows you to reliably kill every process a service might fork off (sysv
      init scripts can't do this)
- boot times are fast
    - containers can boot in less than 1 sec
- systemd ensures taht _every_ log message from boot is logged - sysv init does
  not have this property
- they aim for 99% compatible with sysv init scripts
- the `systemctl` command will assume `.service` as type if you don't specify
  one
- run-levels are replaced by "targets"
    - `/etc/initab` is no longer used
    - the targets are symlinked back to the runlevels for compability
    - there can be only one run-level active at a time but many targets can be
      active at a time
        - targets are a way to group other kinds of units
- nspawn
    - systemd version of kvm
    - fairly low-level
    - networkd
    - systemd version of networkmanager
- units can have alias names - you can show them via:
  `systemctl show nfs-server.service -p Names`

# ubuntu and booting

- 14.x uses upstart
- In 15.x both upstart and systemd are installed, uses systemd by default
- in 16.x
    - uses systemd
    - upstart is not installed by default
    - /sbin/init (process 1) is a symlink to /lib/systemd/systemd
- Aside: Amazon linux uses upstart (as of 2017-05-22)

## Working with systemd

Sources

- https://www.digitalocean.com/community/tutorials/systemd-essentials-working-with-services-units-and-the-journal
- Really good article:
  https://www.linux.com/learn/understanding-and-using-systemd

```
# list info



# work with a particular system
systemctl VERB OBJECT
systemctl status nginx
systemctl status nginx.service # same as above (.service is default type
systemctl status nginx -l # show last 10 log messages

systemctl enable nginx # enable service to start at boot
# creates a link in /etc/systemd/system to the services' /usr/lib/systemd/system startup script
# so the service will be enabled on boot
systemctl disable nginx # disable service to start at boot

systemctl start [name.service]
systemctl stop [name.service]
systemctl restart [name.service]
systemctl reload [name.service]
systemctl status [name.service]
systemctl is-active [name.service]
systemctl list-units --type service --all


# Working with log messages
journalctl -b # show all messages since boot
journalctl -b -u ssh # show all messages since boot from the ssh.service unit


systemctl get-default
```

> The systemctl utility does not communicate with services that have not been
> started by systemd. When systemd starts a system service, it stores the ID of
> its main process in order to keep track of it. The systemctl utility then uses
> this PID to query and manage the service. Consequently, if a user starts a
> particular daemon directly on the command line, systemctl is unable to
> determine its current status or stop it.

## Mount through systemd

- you can do mount through systemd
- you can also do automounts through it

## Interpreting systemctl list-units

There are 3 possible states

1. enabled
    - has a symlink in a .wants directory
1. disabled
    - does not have a symlink in a .wants directory
1. static
    - this unit has not `[Install]` section in its init script so it cannot be
      installed
    - this unit is intended to be a dependency of another unit and is not
      intended to be started directly
