# FileVault 2

Good source: http://www.tekrevue.com/tip/enable-filevault-encryption-mac/

## About

Need _OS X Recovery_ (apple recovery partition) installed on the drive

* You get a _key_ that can be used to unlock it if  you forget your account password
* You can choose to store that key with apple
    * have to answer 3 security questions if you do and get the key back by ringing applecare

## Downsides

* The data is only locked when the computer is fully shutdown. If it is just
  sleeping a user just has to crack your login password. FileVault 2 is
  transparent to the user after an authorized log in.
* It only encrypts the system drive
* it is not compatible with all raid systems
* Time machine backups of the encrypted drive are not enabled by default (but can be enabled)
* Presumably there disk access is slower. Anecdotally it is not noticible on modern macs.


# Encrypted disk images

## option 1: apple

Create a new disk image in disk utility that has encryption. Options are
* 128 bit AES
* 256 bit AES
You can also "create disk image from folder"


## option 2: truecrypt

not an option anymore.

## Disk image formats

Disk Utility and its companion `hdiutil` can create disk images in a number of formats:

Tools tend to work either at the level of the disk image (e.g. blocks) or at the level of the filesystem (the contents of the disk image. When you double click on a disk image two things happen:

1. It is _attached_ to the system
2. The filesystem on it is _mounted_.

Note it is not correct to say the it is just "mounting the filesystem"

1. Read/write disk image
    * File extension .dmg
    * Takes up a fixed amount of space
2. Sparse disk image
    * File extension .sparseimage
    * Only takes as much space as it needs _up to the maximum capacity set when it was created_.
    * This preassigned limit can be changed with _Disk Utility_ or _hdiutil_
3. Sparse bundle disk image
    * Instead of being a single file it is a directory (bundle)
    * contains many files called _bands_ each about 8MB
    * allows backup software to work more efficiently
    * used by Time Machine
    * If you "delete" data from the bundle it is just marked as deleted in the
      bands files but the bands files are not reduced in size (or number?). You
      can "compact" a sparse bundle using `hdiutil compact Foo.sparsebundle-
      compacting is the first thing Time Machine does when it runs out of
      space.
4. DVD/CD image

Contents of a sparse bundle:
```
    Foo.sparsebundle/
       Info.plist
       bands/
```
