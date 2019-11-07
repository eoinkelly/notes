# Instructions for setting up Rasbian Stretch lite from scratch

# Install

1. Download and `dd` to SD card the latest raspbian https://www.raspberrypi.org/downloads/raspbian/ (Note to self: I chose lite edition)
1. `dd` it to an SD card following https://www.raspberrypi.org/documentation/installation/installing-images/mac.md
    ```bash
    diskutil list

    diskutil unmountDisk /dev/diskN

    # note use of rdiskN not diskN below (for speed)
    dd if=raspbian.img of=/dev/rdiskN bs=1m conv=sync
    ```
1. At this point you will need to login locally
1. Use `rasbpi-config` command to complete setup. Configure:
    * locale
    * wifi
    * turn on SSH server

All further steps can be completed via ssh.


## Download updates

```bash
# update packages
# ###############
sudo apt update
sudo apt dist-upgrade

sudo apt install vim
```

## Install Unifi controller

```bash
# Install Unifi controller
# ########################

sudo apt install dirmngr
sudo apt-key adv --recv-key --keyserver keyserver.ubuntu.com EEA14886
sudo apt-key adv --recv-key --keyserver keyserver.ubuntu.com C2518248EEA14886 # launchpad VLC

# add lines to /etc/apt/sources.list.d/java.list
deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main
deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main

apt update


sudo systemctl disable mongodb

sudo apt-get -y install oracle-java8-jdk

# OLD echo 'deb http://www.ubnt.com/downloads/unifi/debian stable ubiquiti' | sudo tee -a /etc/apt/sources.list.d/100-ubnt.list > /dev/null
# OLD sudo wget -O /etc/apt/trusted.gpg.d/unifi-repo.gpg https://dl.ubnt.com/unifi/unifi-repo.gpg


echo 'deb http://www.ui.com/downloads/unifi/debian stable ubiquiti' | sudo tee /etc/apt/sources.list.d/100-ubnt-unifi.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6
echo "deb http://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/3.4 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.4.list



sudo apt-get update

# makes startup quicker by compensating for lack of mouse movement randomness
sudo apt-get install haveged -y

sudo apt-get -y install unifi

# stop the system mongo because unifi runs its own instance of mongo
sudo systemctl stop mongodb
sudo systemctl disable mongodb

# check that all is good
sudo systemctl status unifi

# Visit https://IP_OF_THE_PI:8443/ in browser

# Note: As of 2019-01-14 the Unifi controller backup/restore does not backup
# the admin user so you should create the user with the first-run wizard and then
# do a restore from the settings panel
```

## Setup external USB to auto-mount

```
# exfat stuff required to read external exfat disks
sudo apt install exfat-fuse exfat-utils

# append to /etc/fstab
PARTUUID=f116e4fe-01 /media/backups exfat defaults,nofail 0 0
```

## Setup samba (time machine backup)

Time machine uses SMB shares so setup Samba. Instructions: https://www.raspberrypi.org/documentation/remote-access/samba.md

Samba since 4.8 can be a time machine network share

```
# https://kirb.me/2018/03/24/using-samba-as-a-time-machine-network-server.html

cd ~
mkdir build
cd build/
wget --content-disposition https://github.com/samba-team/samba/archive/samba-4.9.2.tar.gz
tar zxvf samba-samba-4.9.2.tar.gz

sudo apt install build-essential avahi-daemon tracker libtracker-sparql-1.0-dev

DEB_HOST_MULTIARCH=$(dpkg-architecture -qDEB_HOST_MULTIARCH)

export DEB_HOST_MULTIARCH=$(dpkg-architecture -qDEB_HOST_MULTIARCH)

cd samba-samba-4.9.2/

sudo apt install python-dev
sudo apt install libjansson-dev
sudo apt install libgnutls-dev
sudo apt install gnutls-devel
sudo apt install libgnutls28-dev
sudo apt install libgpgme-dev
sudo apt install libarchive-dev
sudo apt install libacl1-dev
sudo apt install libldap2-dev

# https://wiki.samba.org/index.php/Package_Dependencies_Required_to_Build_Samba#Debian_.2F_Ubuntu

sudo apt-get install acl attr autoconf bind9utils bison build-essential \
  debhelper dnsutils docbook-xml docbook-xsl flex gdb libjansson-dev krb5-user \
  libacl1-dev libaio-dev libarchive-dev libattr1-dev libblkid-dev libbsd-dev \
  libcap-dev libcups2-dev libgnutls28-dev libgpgme-dev libjson-perl \
  libldap2-dev libncurses5-dev libpam0g-dev libparse-yapp-perl \
  libpopt-dev libreadline-dev nettle-dev perl perl-modules pkg-config \
  python-all-dev python-crypto python-dbg python-dev python-dnspython \
  python3-dnspython python-gpgme python3-gpgme python-markdown python3-markdown \
  python3-dev xsltproc zlib1g-dev liblmdb-dev lmdb-utils

sudo apt-get install libsystemd-dev

./configure     --prefix=/usr --exec-prefix=/usr --sysconfdir=/etc     --localstatedir=/var --libdir=/usr/lib/$DEB_HOST_MULTIARCH     --with-privatedir=/var/lib/samba/private     --with-smbpasswd-file=/etc/samba/smbpasswd     --enable-fhs --enable-spotlight --with-systemd

```


/usr/lib/jvm/jdk-8-oracle-arm32-vfp-hflt/jre/bin/java -Dfile.encoding=UTF-8 -Djava.awt.headless=true -Dapple.awt.UIElement=true -Xmx1024M -XX:+ExitOnOutOfMemoryError -XX:+CrashOnOutOfMemoryError -XX:ErrorFile=/usr/lib/unifi/logs/hs_err_pid%p.log -jar /usr/lib/unifi/lib/ace.jar start
