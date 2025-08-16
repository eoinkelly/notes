# runit

Good overview: http://www.mikeperham.com/2014/07/07/use-runit/

From the apt package

> Description: system-wide service supervision runit is a collection of tools to
> provide system-wide service supervision and to manage services. Contrary to
> sysv init, it not only cares about starting and stopping services, but also
> supervises the service daemons while they are running. Amongst other things,
> it provides a reliable interface to send signals to service daemons without
> the need for pid-files, and a log facility with automatic log file rotation
> and disk space limits.
>
> runit service supervision can run under sysv init or replace the init system
> completely. Complete init replacement requires some manual configuration
> described in the supplied documentation.

Key points

- runit does not usually replace your init system but lives along side whatever
  init system you have
- runit can be made to replace your init system but that seems overly fussy
- runit will also supervise the processes while they are running - it isn't just
  a starter & stopper

- pros/cons
    - ++ simple, not much code
    - ++ easy to understand setup
    - ++ does automatic restart of processes
    - ++ manages pid files for you (they live in the `service-name/supervise`
      dir)
    - -- doesn't notify when a process needs to be restarted (the process may
      write to the log file but not guaranteed)
- helper programs
    - runsv path/to/service/dir
        - starts and monitors a service an optionally an "attendant" log service
        - you don't usually run this directly - runsvdir does it for you
        - not that runsv doesn't monitor service dirs for changes - runsvdir
          does that
        - example
            ```
            cd path/to/servicedir
            ./run
            ```
    - runsvdir
        - monitors the given directory and starts a runsv for each symlinked
          directory in it
    - sv
        - sends signals to services by name (admin doesn't need pid file)
        - used by admins to start/stop/restart services
        - sv is designed so an sysv init script can invoke it to start a service
- runit is a cross-platform Unix init scheme with service supervision, a
  replacement for sysvinit, and other init schemes
- runit is intended to run as Unix process no 1, it is automatically started by
  the runit-init /sbin/init-replacement if this is started by the kernel
- seems to be prefered by some users as being conceptually simpler and more
  reliable that upstart
- apparently there are some problems with upstart on amazon EC2 instances
- runit is not installed by default on ubuntu - `apt-get install runit` to get
  it
- chef integration
    - runit is used by Opscode to manage all their private chef stuff. Used for
      anything that isn't started by the OS
    - Opscode provide the runit cookbook
      https://supermarket.chef.io/cookbooks/runit

# File layout

```
/etc/service
# contains symbolic links to **directories** under /etc/sv/
# Any service linked under /etc/service will be started at boot by runit
# example
# /etc/service/runsvdir-deploy/ -> /etc/sv/runsvdir-deploy/

/etc/sv
# contains system wide service definitions
# each service definition is a dir with
#   1. a `run` executable script
#   1. a `log` dir which contains config for logging
#   1. a `supervise` dir which contains working files which allow runit to work

# Chef runit sets up a service in /etc/sv which runs runsvdir on
# ~/deploy/service which in turn loads services from ~deploy/sv
/etc/sv/runsvdir-deploy
/etc/sv/runsvdir-deploy/log/
/etc/sv/runsvdir-deploy/supervise/
/etc/sv/runsvdir-deploy/run

/home/deploy/service/{appname} -> /home/deploy/sv/{appname}
/home/deploy/sv/{appname}/run
/home/deploy/sv/{appname}/log/
/home/deploy/sv/{appname}/supervise/

# contains the log of process starting and stopping
/var/log/{appname}/current
```

## What it looks like in htop

- when tree-view enabled you should see `runsvdir` as the parent node somewhere
    - under that will be one or more `runsv` processes
        - under those may be some `svlogd` processes which `runsv` started to
          gather logs from the process

## seeing which processes are running

Options

1. Use sv to get status of services
    ```
    # this will show the services that runit is managing
    sudo sv status /etc/service/*
    sudo sv status ~deploy/service/*
    ```
1. you can infer which processes runit is managing using `pstree -p`. it doesn't
   totally map for unicorn tho because of the slightly odd way unicorn is
   started
1. Use `htop` and hit `t` to enable tree view then filter for runsv
