# Writing ISO files to USB drives on MacOS
```
# convert iso file to dmg
hdiutil convert -format UDRW -o ubuntu ubuntu-14.04.4-desktop-amd64.iso # creates ubuntu.dmg

# now find which disk devic is the USB stick (/dev/disk3 is used in this example)
diskutil list

# unmount it
diskutil unmountDisk /dev/disk3

# write the dmg file onto it
# the "r" is not a typo - it is faster (TODO: find out why
sudo dd if=./ubuntu.dmg of=/dev/rdisk3 bs=1m

# eject the disk - good to go!
diskutil eject /dev/disk3
```
