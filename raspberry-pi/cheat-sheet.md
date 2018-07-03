
# Find the pi on the network

```
sudo nmap -sn 192.168.1.* # run as root to get network interface maker string
ssh pi@192.168.1.74
```

Set locale

```
# https://wiki.debian.org/Locale
sudo dpkg-reconfigure locales
```


Install unifi controller

```
sudo apt-get update
sudo apt-get -y upgrade
sudo apt-get -y install oracle-java8-jdk

# replace 'stable' with 'oldstable' or 'testing' to get those channels instead
echo 'deb http://www.ubnt.com/downloads/unifi/debian stable ubiquiti' | sudo tee -a /etc/apt/sources.list.d/100-ubnt.list > /dev/null

sudo wget -O /etc/apt/trusted.gpg.d/unifi-repo.gpg https://dl.ubnt.com/unifi/unifi-repo.gpg

sudo apt-get update

sudo apt-get -y install unifi

# unifi runs as a systemd service
# it stores data in a local mongodb
systemctl status ???

sudo systemctl disable mongodb
sudo systemctl stop mongodb
```
