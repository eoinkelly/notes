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
