# system V init (oldest, legacy)

From it we get run levels etc.

QUESTION: is this used in _any_ modern linux? when did it stop being used?

- scripts stored in `/etc/init.d`
- links in `/etc/rc0.d` through `/etc/rc6.d`
- each script is a shell script with a particular LSB comment in header between
  `BEGIN INIT INFO` and `END INIT INFO`
    - https://wiki.debian.org/LSBInitScripts
    - the header provides metadata for which run-levels the script should be run
      in

```
#!/bin/sh
# Start/stop the cron daemon.
#
### BEGIN INIT INFO
# Provides:          cron
# Required-Start:    $remote_fs $syslog $time
# Required-Stop:     $remote_fs $syslog $time
# Should-Start:      $network $named slapd autofs ypbind nscd nslcd winbind
# Should-Stop:       $network $named slapd autofs ypbind nscd nslcd winbind
# Default-Start:     2 3 4 5
# Default-Stop:
# Short-Description: Regular background program processing daemon
# Description:       cron is a standard UNIX program that runs user-specified
#                    programs at periodic scheduled times. vixie cron adds a
#                    number of features to the basic UNIX cron, including better
#                    security and more powerful configuration options.
### END INIT INFO
```
