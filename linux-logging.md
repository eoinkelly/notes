### The main bits of logging infrastructure:

1. journald
2. rsyslogd
3. awslog


* `journald` is configured to send all its logs to syslog by default
    * see `#ForwardToSyslog=yes` in `/etc/systemd/journald.conf`

* journald gets logs from:
   * Kernel log messages, via kmsg
   * Simple system log messages, via the libc syslog(3) call
   * Structured system log messages via the native Journal API, see sd_journal_print(4)
   * Standard output and standard error of system services
   * Audit records, via the audit subsystem

### Sources of messages

* Kernel messages
    * logged to `/proc/kmsg` aka the "kernel ring buffer"
    * `dmesg` is a program that dumps /proc/kmsg. In addition, it provides some filtering capabilities to weed out logs that the user isn't interested in.
    * also sent to `/var/log/kern.log`
* `syslog(3)` libc function
* `sd_journal_print(4)` libc function
    * sends direclty to journald
* anythign else which rsyslogd is configured to pull in

### Important log files

* /var/log/auth.log
    * auth related messages
* /var/log/syslog
    * general messages execpt auth
* /var/log/mail.log
    * mail related messages
    * doesn't exist by default on AWS Ubuntu (probalby because we don't install an MTA)
* /var/log/cloud-init.log
*     * seems to be an AWS thing
* /var/log/cloud-init-output.log
    * seems to be the output of the AWS script which sets up the instance
    * our user-data script output is visible in it until our script redirects its output
* /var/log/messages
    * doesn't exist in modern Ubuntu (it sends everything to `/var/log/syslog`)
