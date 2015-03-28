# process starting and monitoring

Great resource: http://jtimberman.housepub.org/blog/2012/12/29/process-supervision-solved-problem/

# runit

* runit is a cross-platform Unix init scheme with service supervision, a
  replacement for sysvinit, and other init schemes
* runit is intended to run as Unix process no 1, it is automatically started by
  the runit-init /sbin/init-replacement if this is started by the kernel
* runit is a process supervisor
* seems to be prefered by some users as being conceptually simpler and more reliable that upstart
* apparently there are some problems with upstart on amazon EC2 instances
* not installed by default on ubuntu - `apt-get install runit` to get it
* runsv
    * The runsv program supervises services, and will restart them if they
      fail. While it doesn’t provide any notification that the service failed,
      other than possibly writing to the log, this means that if a
      configuration issue caused a service to fail, it will automatically start
      when the configuration file is corrected.

      Each service managed by runsv has a “service directory” where all its
      files are kept. Here, a “supervise” directory is managed by runsv, and a
      “pid” file containing the running PID is stored. However this isn’t the
      same as the pidfile management used in init scripts, and it means program
      authors don’t have to worry about managing a pidfile.

      The sv program is used to send signals to services, and for general
      management of the services. It is used to start, stop and restart
      services. It also implements a number of commands that can be used for
      signals like TERM, CONT, USR1. sv also includes “LSB-init” compatibility,
      so the binary can be linked to /etc/init.d/service-name so “init style”
      commands can be used

* runit is used by Opscode to manage all their private chef stuff. Used for
  anything that isn't started by the OS
* Opscode provide the runit cookbook https://supermarket.chef.io/cookbooks/runit

# File layout

```
/etc/service
/etc/service/runsvdir-deploy -> /etc/sv/runsvdir-deploy

/etc/sv/getty-5/
/etc/sv/getty-5/finish
/etc/sv/getty-5/run
/etc/sv/getty-5/supervise -> /var/run/sv.getty-5

/etc/sv/runsvdir-deploy
/etc/sv/runsvdir-deploy/log/
/etc/sv/runsvdir-deploy/supervise/
/etc/sv/runsvdir-deploy/run

/home/deploy/service/{appname} -> /home/deploy/sv/{appname}
/home/deploy/sv/{appname}/run
/home/deploy/sv/{appname}/log/
/home/deploy/sv/{appname}/supervise/

/var/log/{appname}/current contains the log of process starting and stopping
```

* seeing which processes are running
    * Option 1: you can infer which processes runit is managing using `pstree -p`.
    it doesn't totally map for unicorn tho because of the slightly odd way
    unicorn is started

    * Option 2:
    ```
    # this will show the services that runit is managing (doesn't show unicorns tho)
    sudo sv s /etc/service/*
    ```

# system V init (oldest, legacy)

From it we get run levels etc.

# upstart (newer)

Upstart is an event-based replacement for the /sbin/init daemon which handles
starting of tasks and services during boot, stopping them during shutdown and
supervising them while the system is running.

It was originally developed for the Ubuntu distribution, but is intended to be
suitable for deployment in all Linux distributions as a replacement for the
venerable System-V init.

# systemd (newest)

systemd is a suite of system management daemons, libraries, and utilities
designed as a central management and configuration platform for the Linux
computer operating system. Described by its authors as a "basic building block"
for an operating system,[5] systemd primarily aims to replace the Linux init
system (the first process executed in user space during the Linux startup
process) inherited from UNIX System V and Berkeley Software Distribution (BSD).

systemd is designed for Linux and programmed exclusively for the Linux API.

The design of systemd generated significant controversy within the free
software community, leading the critics to argue that systemd's architecture
violates the Unix philosophy and that it will eventually form a system of
interlocking dependencies.[7] However, as of 2015 most major Linux
distributions have adopted it as their default init system

Systemd's model for starting processes (units) is "lazy dependency-based"

Upstart's model for starting processes (jobs) is "greedy event-based"

# ubuntu and booting

* 14.x uses upstart, 15.x will move to systemd (15.04 is out in april 2015)
* docs: https://wiki.ubuntu.com/SystemdForUpstartUsers
* In 15.x both upstart and systemd will be installed

# other options for system starting

* supervisord http://supervisord.org/
* daemontools
* circus http://circus.readthedocs.org/en/0.11.1/
* monit
* god
* foreman
* bluepill
