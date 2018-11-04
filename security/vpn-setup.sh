#!/bin/bash

# based on
# https://www.digitalocean.com/community/tutorials/how-to-set-up-an-openvpn-server-on-ubuntu-16-04
set -e

sudo apt-get update
sudo apt-get upgrade
sudo locale-gen en_NZ.UTF-8

# install handy stuff
sudo apt-get elinks tree vim htop

sudo apt-get install elinks
elinks https://www.google.co.uk

sudo apt-get install openvpn easy-rsa
make-cadir ~/openvpn-ca
vim ~/openvpn-ca/vars
cd openvpn-ca/
source vars
./clean-all
./build-ca
./build-key-server eoin-vpn-server
cd openvpn-ca/
source vars
./clean-all
./build-ca
./build-key-server eoin-vpn-server
./build-dh
openvpn --genkey --secret keys/ta.key
./build-key client1
cd keys/
sudo cp ca.crt eoin-vpn-server.crt eoin-vpn-server.key ta.key dh2048.pem /etc/openvpn
gunzip -c /usr/share/doc/openvpn/examples/sample-config-files/server.conf.gz | sudo tee /etc/openvpn/server.conf
sudo vim /etc/openvpn/server.conf
sudo vim /etc/sysctl.conf
sudo sysctl -p
ip route | grep default
sudo ufw
sudo ufw status
sudo systemctl start openvpn@server
sudo systemctl status openvpn@server
ip addr show tun0
sudo systemctl enable openvpn@server
mkdir -p ~/client-configs/files
chmod 700 ~/client-configs/files
cp /usr/share/doc/openvpn/examples/sample-config-files/client.conf ~/client-configs/base.conf
vim ~/client-configs/base.conf
cd ~/client-configs/
vim ~/client-configs/make_config.sh
chmod 700 ~/client-configs/make_config.sh
./make_config.sh client1
# sudo journalctl -u openvpn@server

sudo vim /etc/openvpn/server.conf
echo 1 | sudo tee /proc/sys/net/ipv4/ip_forward
sudo systemctl openvpn
sudo systemctl status openvpn
sudo systemctl status openvpn@server
sudo systemctl restart openvpn@server
sudo systemctl status openvpn@server
sudo iptables -t nat -A POSTROUTING -s 10.4.0.1/2 -o eth0 -j MASQUERADE
sudo iptables -t nat -A POSTROUTING -s 10.8.0.0/24 -o eth0 -j MASQUERADE
sudo systemctl restart openvpn
sudo systemctl restart openvpn@server
ps aux|grep vpn
time curl https://www.bbc.co.uk
ping www.bbc.co.uk
htop
cat /run/openvpn/server.pid
ls -l /run/openvpn
cat /etc/openvpn/server.conf
less base.conf
cat make_config.sh
ps aux|grep openvpn
vim /etc/openvpn/server.conf
cd /var/log
cd /etc/openvpn
sudo iptables -t nat -A POSTROUTING -s 10.4.0.1/2 -o eth0 -j MASQUERADE
sudo iptables -t nat -A POSTROUTING -s 10.8.0.0/24 -o eth0 -j MASQUERADE
sudo systemctl restart openvpn@server
cd ~
vim setup_nat.sh
chmod u+x setup_nat.sh
./setup_nat.sh
cd /etc/openvpn
cat openvpn-status.log
sudo su
./setup_nat.sh
cat setup_nat.sh
sudo iptables -L
sudo iptables -L -v
sudo iptables -t nat -A POSTROUTING -s 10.4.0.1/2 -o eth0 -j MASQUERADE
sudo iptables -L -v
sudo apt-et install iptables-persistent
sudo apt-get install iptables-persistent
cat /etc/iptables/rules.v4
rm setup_nat.sh
sudo apt-get install unattended-upgrades

# install AWS cloudwatch


sudo apt-get update
sudo apt-get install awslogs
sudo apt-get install awslog
curl https://s3.amazonaws.com/aws-cloudwatch/downloads/latest/awslogs-agent-setup.py -O
sudo python ./awslogs-agent-setup.py --region us-east-1
sudo python3 ./awslogs-agent-setup.py --region eu-west-2
vim /var/awslogs/etc/awslogs.conf
sudo vim /var/awslogs/etc/awslogs.conf
