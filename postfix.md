# Postfix

* outbound mail stored in sub-directories of `/var/spool/postfix/`
    * there are a bunch of dirs
* inbound mail stored in
    * `/var/spool/mail/<USERNAME>`
        * `mbox` format (all mail in one text file, mails delimited by files starting with `From:`)
    * `/var/mail/<USERNAME>`
        * On RHEL this is a symlink to `var/spool/mail`
* `mailq` is a postfix to sendmail compatibility layer


```
# check if it has been installed
$ rpm -q postfix
postfix-2.10.1-6.el7.x86_64

$ systemctl status postfix

   CGroup: /system.slice/postfix.service
           ├─ 1508 /usr/libexec/postfix/master -w
           ├─ 1513 qmgr -l -t unix -u
           └─14966 pickup -l -t unix -u

$ ps -eZ |grep postfix
system_u:system_r:postfix_master_t:s0 1508 ?   00:00:15 master
system_u:system_r:postfix_qmgr_t:s0 1513 ?     00:00:03 qmgr
system_u:system_r:postfix_pickup_t:s0 14966 ?  00:00:00 pickup

$ echo "Subject: test12" | sendmail -v eoin.kelly@nzx.com
$ sendmail -vt < ~/mail.txt

# sending an email manually
$ telnet localhost 25
EHLO wlt-sed-app1.localdomain
MAIL FROM:ekelly@wlt-sed-app1.localdomain
RCPT TO:eoin.kelly@nzx.com
DATA
Subject: test manually

this is a test message

.

quit


# inspect postfix queue
# show queued messages (sender, recipients, message ID)
postqueue -p
# or
mailq # on older installations postqueue is not available

# see full contents of a message in the queue
posqcat -vq MSG_ID

postqueue -f # flush the queue

# More
# http://www.tech-g.com/2012/07/15/inspecting-postfixs-email-queue/
```

