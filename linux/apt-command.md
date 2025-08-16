- designed to be used by humans - don't use it for scripts because the interface
  might change
- all features of it are avaialble in the underlying tools

```
apt update
apt upgrade
apt install
apt full-upgrade
apt remove
apt purge
apt autoremove
apt search
apt show
apt edit-sources

```

## listing packages

```
apt list
apt list --installed
apt list --upgradable
apt list --all-versions
```

- similar to `dpkg-query --list`
- supports glob patterns for package names

Output has the form:

```
{PACKAGE_NAME}/{X} {UBUNTU_PACKAGE_VERSION} {ARCH} [{STATUS}]

X
    * comma separated list of ???
    ? maybe is the section or the ppa name???
    * examples
        bionic,now
        bionic-updates
ARCH
    * architecture this package is built for
    * example values:
        amd64
        all

UBUNTU_PACKAGE_VERSION
    * the specific version of the deb file installed on the system

STATUS:
    * possible values:
        installed
        upgradable from: {OLD_VERSION}
```

Example:

```
$ apt list --upgradable
tmux/bionic-updates 2.6-3ubuntu0.1 amd64 [upgradable from: 2.6-3]
ubuntu-keyring/bionic-updates 2018.09.18.1~18.04.0 all [upgradable from: 2018.02.28]
ubuntu-release-upgrader-core/bionic-updates 1:18.04.30 all [upgradable from: 1:18.04.17]
udev/bionic-updates 237-3ubuntu10.15 amd64 [upgradable from: 237-3ubuntu10]
unattended-upgrades/bionic-updates 1.1ubuntu1.18.04.9 all [upgradable from: 1.1ubuntu1]
update-manager-core/bionic-updates 1:18.04.11.9 all [upgradable from: 1:18.04.11]
update-notifier-common/bionic-updates 3.192.1.5 all [upgradable from: 3.192]
util-linux/bionic-updates 2.31.1-0.4ubuntu3.3 amd64 [upgradable from: 2.31.1-0.4ubuntu3]
uuid-runtime/bionic-updates 2.31.1-0.4ubuntu3.3 amd64 [upgradable from: 2.31.1-0.4ubuntu3]
```
