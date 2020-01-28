# Cron

* Often implemented by systemd now
* Handy for working out syntax: https://crontab.guru/

## System crontab /etc/crontab

* `/etc/crontab`
    * it's not recommended you edit this
    * doesn't have to be edited by `crontab`
    * it has a different format to user crontab files
    * typically runs anacron on the /etc/cron.{daily|monthly|weekly} directories
* `/etc/cron.d`
    * gets picked up by the cron daemon
    * contains cron files cron format that includes the username to run as
    * custom crons added by provisioning tools should put their scripts in here!

Example  /etc/cron.d/ script

```
SHELL=/bin/sh
PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
47 15 * * *   root    test -x /etc/cron.daily/popularity-contest && /etc/cron.daily/popularity-contest --crond
```

```
$ ls -l /etc/|grep cron
drwxr-xr-x  2 root root    4096 Nov 27 01:10 cron.d
drwxr-xr-x  2 root root    4096 Nov 27 06:27 cron.daily
drwxr-xr-x  2 root root    4096 Sep 13 11:36 cron.hourly
drwxr-xr-x  2 root root    4096 Sep 13 11:36 cron.monthly
-rw-r--r--  1 root root     722 Apr  5  2016 crontab
drwxr-xr-x  2 root root    4096 Sep 13 11:38 cron.weekly
```

## user crontabs (not commonly used on servers)

* crontab is the program used to manage the `cron` daemon
* User crontabs stored in `/var/spool/cron/crontabs` but should be edited via `crontab` command
* /etc/cron.allow # says users who can usecrontab (takes precedence over /etc/cron.deny)
* /etc/cron.deny # lists users who are not allowed to use crontab
* root is always allowed to use cron
* su can confuse crontab so always use `-u`



```
# show crontab file
crontab -l  # current user
crontab -u otheruser -l # 'otheruser' user

# remove all cron jobs
crontab -r

# edit crontab file
crontab -e
crontab -u otheruser -e
```

You can use the following strings to define a job run in a user crontab

```
@reboot	Run once, at startup.
@yearly	Run once a year.
@annually	(same as @yearly).
@monthly	Run once a month.
@weekly	Run once a week.
@daily	Run once a day.
@midnight	(same as @daily).
@hourly	Run once an hour.
```
