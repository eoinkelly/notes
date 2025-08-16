# AuditD

Sources

- https://www.digitalocean.com/community/tutorials/how-to-write-custom-system-audit-rules-on-centos-7

```bash

$ auditctl -l # list rules

$ auditctl -S # show status (one line)
# AUDIT_STATUS: enabled=1 flag=1 pid=9736 rate_limit=0 backlog_limit=320 lost=0 backlog=0
# backlog = num events queued up for auditd to read
# flag = how failures handled (0=silent, 1=printk. 2=panic), default = 1

auditctl -a .... # insert rule at bottom of list
auditctl -A .... # insert rule at top of list

# show all system call names (for creating rules)
ausyscall --dump


the rules file
/etc/audit/rules.d/audit.rules


Filesystem watch rule
$ auditctl -w path_to_file -p permissions -k key_name
# key_name = identify the rule in the log
# permissions = a combination of r(read), w(write), x(execute), and a(attribute change)
#     which permission changes to log
# w = log anytime a process writes to the file
# a = log anytime process changes file attrs
# path_to_file = can be a file or a directory. if directory then watch all files and dirs underneath it, wildcards not supported


System call watch rule
$ auditctl -a action,filter -S system_call -F field=value -k key_name`

action = always|never (turn auditing on/off, can override earlier rules)
filter=name of a kernel rule matching filter, one of: task|exit|user|exclude
    always,exit is the most common combo, says "audit this system call when it exits"

-S system_call
    = name of the system call to audit
    can have multiple -S args in a rule
    'all' special name meaning all

-F field=value
    additional options that modify the rule
    a specific
        architecture
        user ID
        process ID
        path
        ... others ...
    examples
        -F arch=b64 # audit the 64bit version of this system call
        -F "auid>=1000" # log if user id >= 1000
        -F "auid=1234" # log if user id == 1234
        -F path=/etc/hosts # alterntive syntax used to define a filesystem watcher rule
        -F path=/path/to/dir # alterntive syntax used to define a filesystem watcher rule to recursively watch a dir
-k key_name = optional, allows you to identify rule in the logs

# these do the same thing
-a always,exit -F path=/etc/hosts -F perm=wa -k hosts_file_change
-w /etc/hosts -p wa -k hosts_file_change

```

    -a always,exit -F arch=b64 -S sethostname -S setdomainname -k system-locale
    -a always,exit -F arch=b32 -S sethostname -S setdomainname -k system-locale
    -w /etc/issue -p wa -k system-locale
    -w /etc/issue.net -p wa -k system-locale
    -w /etc/hosts -p wa -k system-locale
    -w /etc/network -p wa -k system-local

### Annotated rules

```bash
# create log entry if anything writes
-w /etc/apparmor -p wa -k MAC-policy
-w /etc/apparmor.d -p wa -k MAC-policy

-a always,exit -F arch=b64 -S open,truncate,ftruncate,creat,openat -F exit=-EACCES -F auid>=1000 -F auid!=-1 -F key=access
-a always,exit -F arch=b32 -S open,creat,truncate,ftruncate,openat -F exit=-EACCES -F auid>=1000 -F auid!=-1 -F key=access
-a always,exit -F arch=b64 -S open,truncate,ftruncate,creat,openat -F exit=-EPERM -F auid>=1000 -F auid!=-1 -F key=access
-a always,exit -F arch=b32 -S open,creat,truncate,ftruncate,openat -F exit=-EPERM -F auid>=1000 -F auid!=-1 -F key=access
-a always,exit -F arch=b64 -S execve -C uid!=euid -F euid=0 -F auid>=1000 -F auid!=-1 -F key=actions
-a always,exit -F arch=b32 -S execve -C uid!=euid -F euid=0 -F auid>=1000 -F auid!=-1 -F key=actions
-a always,exit -F arch=b64 -S rename,unlink,unlinkat,renameat -F auid>=1000 -F auid!=-1 -F key=delete
-a always,exit -F arch=b32 -S unlink,rename,unlinkat,renameat -F auid>=1000 -F auid!=-1 -F key=delete
-w /etc/group -p wa -k identity
-w /etc/passwd -p wa -k identity
-w /etc/gshadow -p wa -k identity
-w /etc/shadow -p wa -k identity
-w /etc/security/opasswd -p wa -k identity
-w /var/log/faillog -p wa -k logins
-w /var/log/lastlog -p wa -k logins
-w /var/log/tallylog -p wa -k logins
-w /sbin/insmod -p x -k modules
-w /sbin/rmmod -p x -k modules
-w /sbin/modprobe -p x -k modules
-a always,exit -F arch=b64 -S init_module,delete_module -F key=modules
-a always,exit -F arch=b64 -S mount -F auid>=1000 -F auid!=-1 -F key=mounts
-a always,exit -F arch=b32 -S mount -F auid>=1000 -F auid!=-1 -F key=mounts
-a always,exit -F arch=b64 -S chmod,fchmod,fchmodat -F auid>=1000 -F auid!=-1 -F key=perm_mod
-a always,exit -F arch=b32 -S chmod,fchmod,fchmodat -F auid>=1000 -F auid!=-1 -F key=perm_mod
-a always,exit -F arch=b64 -S chown,fchown,lchown,fchownat -F auid>=1000 -F auid!=-1 -F key=perm_mod
-a always,exit -F arch=b32 -S lchown,fchown,chown,fchownat -F auid>=1000 -F auid!=-1 -F key=perm_mod
-a always,exit -F arch=b64 -S setxattr,lsetxattr,fsetxattr,removexattr,lremovexattr,fremovexattr -F auid>=1000 -F auid!=-1 -F key=perm_mod
-a always,exit -F arch=b32 -S setxattr,lsetxattr,fsetxattr,removexattr,lremovexattr,fremovexattr -F auid>=1000 -F auid!=-1 -F key=perm_mod
```

## Examples

```bash
$ touch eoin.txt

no log generated

$ chmod 0600 eoin.txt

type=SYSCALL msg=audit(1631919388.988:451): arch=c000003e syscall=268 success=yes exit=0 a0=ffffff9c a1=55b52331b1b0 a2=180 a3=fff items=1 ppid=29674 pid=29771 auid=1000 uid=1000 gid=1000 euid=1000 suid=1000 fsuid=1000 egid=1000 sgid=1000 fsgid=1000 tty=pts2 ses=3 comm="chmod" exe="/usr/bin/chmod" subj=unconfined key="perm_mod"
type=CWD msg=audit(1631919388.988:451): cwd="/home/deploy"
type=PATH msg=audit(1631919388.988:451): item=0 name="eoin.txt" inode=262773 dev=103:01 mode=0100664 ouid=1000 ogid=1000 rdev=00:00 nametype=NORMAL cap_fp=0 cap_fi=0 cap_fe=0 cap_fver=0 cap_frootid=0
type=PROCTITLE msg=audit(1631919388.988:451): proctitle=63686D6F64003036303000656F696E2E747874


$ rm eoin.txt

type=SYSCALL msg=audit(1631919513.001:452): arch=c000003e syscall=263 success=yes exit=0 a0=ffffff9c a1=55ff82d37180 a2=0 a3=0 items=2 ppid=29674 pid=29782 auid=1000 uid=1000 gid=1000 euid=1000 suid=1000 fsuid=1000 egid=1000 sgid=1000 fsgid=1000 tty=pts2 ses=3 comm="rm" exe="/usr/bin/rm" subj=unconfined key="delete"
type=CWD msg=audit(1631919513.001:452): cwd="/home/deploy"
type=PATH msg=audit(1631919513.001:452): item=0 name="/home/deploy" inode=256109 dev=103:01 mode=040755 ouid=1000 ogid=1000 rdev=00:00 nametype=PARENT cap_fp=0 cap_fi=0 cap_fe=0 cap_fver=0 cap_frootid=0
type=PATH msg=audit(1631919513.001:452): item=1 name="eoin.txt" inode=262773 dev=103:01 mode=0100600 ouid=1000 ogid=1000 rdev=00:00 nametype=DELETE cap_fp=0 cap_fi=0 cap_fe=0 cap_fver=0 cap_frootid=0
type=PROCTITLE msg=audit(1631919513.001:452): proctitle=726D00656F696E2E747874
```
