
# Minion

* runs two copies of itself like this

```
usr/bin/python /usr/bin/salt-minion
usr/bin/python /usr/bin/salt-minion

```

By default the Salt master listens on ports 4505 and 4506 on all interfaces (0.0.0.0).

* doesn't seem to use SSH to communicate between master and minion

States are stored in text files on the master and transferred to the minions on demand via the master's File Server. The collection of state files make up the State Tree

The core of the Salt State system is the SLS, or SaLt State file

$ salt {MINION_TARGETS} {EXECUTION_MODULE}.{FUNCTION}
$ salt '*' test.ping # run the ping function in the test module on all minions
$ salt '*' sys.doc # show function docs
$ salt '*' disk.usage # show disk usage
$ salt '*' pkg.install vim # install vim on all minions using appropriate pkg manager
$ salt '*' cmd.run 'ls -al /boot' # shell out to a command on all minions
