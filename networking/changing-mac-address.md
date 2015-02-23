# Change MAC address on OSX

```
# To disassociate with current network connection
sudo /System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -z

sudo ifconfig en0
# read the existing mac address and just increment some number

# change mac address
sudo ifconfig en0 ether $new_mac
```
