
```
sudo apt install aide
[sudo] password for deploy:
Reading package lists... Done
Building dependency tree
Reading state information... Done
The following additional packages will be installed:
  aide-common bsd-mailx liblockfile-bin liblockfile1 postfix ssl-cert
Suggested packages:
  procmail postfix-mysql postfix-pgsql postfix-ldap postfix-pcre postfix-lmdb postfix-sqlite sasl2-bin dovecot-common resolvconf postfix-cdb postfix-doc openssl-blacklist
The following NEW packages will be installed:
  aide aide-common bsd-mailx liblockfile-bin liblockfile1 postfix ssl-cert
0 upgraded, 7 newly installed, 0 to remove and 0 not upgraded.
Need to get 2094 kB of archives.
After this operation, 7004 kB of additional disk space will be used.
```

the postfix config wanted me to make choices aobut where mail is sent - how does that work non-interactively?

Why does it install so much mail stuff?
    postfix and mailx?

