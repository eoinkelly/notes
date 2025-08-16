# Difference between su and sudo

- Unlike su(1), when sudo requires authentication, it validates the invoking
  user's credentials, not the target user's (or root's) credentials.
- This can be changed via the rootpw, targetpw and runaspw Defaults entries in
  sudoers.

`su` checks the _target_ user's credentials. `sudo` checks the _invoking_ user's
credentials

The sudoers file settings affect the following execution parameters:

- real and effective user ID
- real and effective group ID
- supplementary group IDs
- the environment list
- file creation mode mask (umask)

# Credential caching

- sudo uses time stamp files for credential caching.
- Once a user has been authenticated, the time stamp is updated and the user may
  then use sudo without a password for a short period of time (5 minutes unless
  overridden by the timeout option) .
- By default, sudo uses a tty-based time stamp which means that there is a
  separate time stamp for each of a user's login sessions.
- The tty_tickets option can be disabled to force the use of a single time stamp
  for all of a user's sessions.

# Hints

```
# show defaults and commands for current user
sudo -l

Matching Defaults entries for eoin on www:
    secure_path=/usr/local/sbin\:/usr/local/bin\:/usr/sbin\:/usr/bin\:/sbin\:/bin,
    env_keep+=SSH_AUTH_SOCK
User eoin may run the following commands on www:
    (ALL) ALL

# show the defaults and commands that a particular user can run
sudo -U eoin -l

Matching Defaults entries for eoin on someserver:
    secure_path=/usr/local/sbin\:/usr/local/bin\:/usr/sbin\:/usr/bin\:/sbin\:/bin,
    env_keep+=SSH_AUTH_SOCK

User eoin may run the following commands on someserver:
    (ALL) NOPASSWD: ALL
```

- sudo requires that most users authenticate themselves by default.
- A password is not required if the invoking user
    1. is root
    2. if the target user is the same as the invoking user
    3. if the authentication has been disabled for the user or command in the
       sudoers file.

## /etc/sudoers

- the default policy file for sudo
- edit with `visudo` as this does some format checking - breaking sudo would be
  bad
- CAUDTION: if there are conflicting values the last value applied in the file
  wins!
- comments prefix by `#` EXCEPT `#include` is a directive to include a file and
  is not a comment! W.T.F.
- contains
    1. aliases (4 kinds)
        - basically variable definitions
        - can have of 4 types
            - User_Alias
                - used to create groups of users
                - users don't have a prefix e.g. `deploy`
                - groups have a `%` prefix e.g. `%www`
                - netgroups have a `+` prefix e.g. `+somegroup`
                - can prefix with `!` to exclude that user/group/netgroup
            - Runas_Alias
                - lets you specify groups of users whom a given User@Host
                  combination can run processes as
                - similar to user aliases but you can specify user by uid
                - uids have a `#` prefix e.g. `#0` would be root (note this
                  isn't a comment)
            - Cmnd_Alias
                - lists of commands and directories
                - used to specify groups of commands
                - if you specify a dir it will include all files in that dir but
                  not subdirs
            - Host_Alias
                - a list of
                    - hostnames,
                    - ip addresses,
                    - networks: if you don't specify netmask the netmask of this
                      server will be used
                    - netgroups
        - you can define multiple aliases on same line, `:` is the separator
        - each type has the `ALL` built-in alias that matches all users, all
          hosts etc.
        - you cannot override the ALL aliases
        - sudo knows which ALL you mean by the context of its use
    2. user specifications are of form
       `who where = (as_whom) tag_constraints what`.
        - details:

            ```
            <user list> <host list> = <operator (runas) list> <tags> <command list>
            User_Spec ::= User_List Host_List '=' Cmnd_Spec_List \
                            (':' Host_List '=' Cmnd_Spec_List)*

            Cmnd_Spec_List ::= Cmnd_Spec |
                                Cmnd_Spec ',' Cmnd_Spec_List

            Cmnd_Spec ::= Runas_Spec? Tag_Spec* Cmnd

            Runas_Spec ::= '(' Runas_List? (':' Runas_List)? ')'

            Tag_Spec ::= ('EXEC:' | 'NOEXEC:' | 'FOLLOW:' | 'NOFOLLOW' |
                        'LOG_INPUT:' | 'NOLOG_INPUT:' | 'LOG_OUTPUT:' |
                        'NOLOG_OUTPUT:' | 'MAIL:' | 'NOMAIL:' | 'PASSWD:' |
                        'NOPASSWD:' | 'SETENV:' | 'NOSETENV:')
            ```

        - tags allow you to set special instructions for each user specification
          line e.g.
            - tags end in `:`
            - examples:
                - PASSWD:
                    - user must enter a password
                - NOPASSWD:
                    - user does not have to enter a password
                - NOEXEC:
                    - don't allow creating of a shell

```
# exclude joe
User_Alias ADMINS %admin, eoin, +somegroup !joe

# #0 is uid 0 here not a comment
Runas_Alias ROOT #0


Host_Alias SERVERS = 192.168.0.1, 192.168.0.2, server1

Host_Alias INTERNAL_NETWORK = 192.168.0.0/255.255.255.0

# And this is every machine in the network that is not a server
Host_Alias WORKSTATIONS = NETWORK, !SERVER



# User privilege specification
<root user> on <all hosts> = running as <any user> with <no tag restriction> can access <all commands>
root    ALL=(ALL) ALL

# Members of the admin group may gain root privileges
%admin ALL=(ALL) ALL

# note that you don't have to create aliases - you can use users, hostnames
# etc. directly inline in the user specifications

myuser ALL = (root) NOPASSWD:NOEXEC: /usr/bin/vim
# lets myuser run vim as root without a pssword but vim cannot shell out via :shell
```

Another example

```
# This file is managed by Chef.
# Do NOT modify this file directly.

Defaults      secure_path="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
Defaults      env_keep+=SSH_AUTH_SOCK

# User privilege specification
root          ALL=(ALL) ALL


vagrant ALL=(ALL) NOPASSWD:ALL
rabidadmin ALL=(ALL) NOPASSWD:ALL

# Members of the group 'admin' may gain root privileges
%admin ALL=(ALL) NOPASSWD:ALL
```

Default MacOS sudoers file:

```
## sudoers file.
##
## This file MUST be edited with the 'visudo' command as root.
## Failure to use 'visudo' may result in syntax or file permission errors
## that prevent sudo from running.
##
## See the sudoers man page for the details on how to write a sudoers file.
##

##
## Host alias specification
##
## Groups of machines. These may include host names (optionally with wildcards),
## IP addresses, network numbers or netgroups.
# Host_Alias	WEBSERVERS = www1, www2, www3

##
## User alias specification
##
## Groups of users.  These may consist of user names, uids, Unix groups,
## or netgroups.
# User_Alias	ADMINS = millert, dowdy, mikef

##
## Cmnd alias specification
##
## Groups of commands.  Often used to group related commands together.
# Cmnd_Alias	PROCESSES = /usr/bin/nice, /bin/kill, /usr/bin/renice, \
# 			    /usr/bin/pkill, /usr/bin/top

##
## Defaults specification
##

Defaults    env_reset
Defaults    env_keep += "BLOCKSIZE"
Defaults    env_keep += "COLORFGBG COLORTERM"
Defaults    env_keep += "__CF_USER_TEXT_ENCODING"
Defaults    env_keep += "CHARSET LANG LANGUAGE LC_ALL LC_COLLATE LC_CTYPE"
Defaults    env_keep += "LC_MESSAGES LC_MONETARY LC_NUMERIC LC_TIME"
Defaults    env_keep += "LINES COLUMNS"
Defaults    env_keep += "LSCOLORS"
Defaults    env_keep += "SSH_AUTH_SOCK"
Defaults    env_keep += "TZ"
Defaults    env_keep += "DISPLAY XAUTHORIZATION XAUTHORITY"
Defaults    env_keep += "EDITOR VISUAL"
Defaults    env_keep += "HOME MAIL"

Defaults    lecture_file = "/etc/sudo_lecture"

##
## Runas alias specification
##

##
## User privilege specification
##
root ALL=(ALL) ALL
%admin  ALL=(ALL) ALL

## Uncomment to allow members of group wheel to execute any command
# %wheel ALL=(ALL) ALL

## Same thing without a password
# %wheel ALL=(ALL) NOPASSWD: ALL

## Uncomment to allow members of group sudo to execute any command
# %sudo	ALL=(ALL) ALL

## Uncomment to allow any user to run sudo if they know the password
## of the user they are running the command as (root by default).
# Defaults targetpw  # Ask for the password of the target user
# ALL ALL=(ALL) ALL  # WARNING: only use this together with 'Defaults targetpw'

## Read drop-in files from /private/etc/sudoers.d
## (the '#' here does not indicate a comment)
#includedir /private/etc/sudoers.d
```

## Options

sudo has many options which can be set in the config file

- env_reset
    - resets the terminal environment after switching to root. So, ie: all user
      set variables are removed
    - is set by default
- pwfeedback
    - shows users password feedback as they type

env_check env_delete use_loginclass

Options can be set by default via the `Defaults` command

```
Defaults        env_reset,pwfeedback
```

A Common setup:

```
$ cat 90-cloud-init-users
# Created by cloud-init v. 19.1-1-gbaa47854-0ubuntu1~18.04.1 on Tue, 11 Jun 2019 07:29:47 +0000

# User rules for deploy
deploy ALL=(ALL) NOPASSWD:ALL
```
