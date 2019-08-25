# Linux stat command

* comes from coreutils
* displays filesystem status
* exists as a shell builtin and standalone binary
* why use it instead of `ls`?
    * handy for shell scripts because you can control output formatting with format sequences
    * gives more detail than `ls` by default:
        * timestamps for atime, ctime, mtime shown with full accuracy
            * Access time (atime) when contents of file were last accessed
            * Modificaiton time (mtime) - when contents of file were last changed
            * Change time (ctime) - when the inode was last modified
            * Birth time - when the file was created (not well supported, waiting on kernel feature)
        * how many hard links exist to the file
        * what device the file lives on
        * fie size but also number of blocks it takes up and IO block size
* See below for details of how to get the file

```bash
$ ls -al .profile
-rw-r--r-- 1 deploy deploy  807 Apr  4  2018 .profile

$ stat .profile
  File: .profile
  Size: 807       	Blocks: 8          IO Block: 4096   regular file
Device: ca01h/51713d	Inode: 256077      Links: 1
Access: (0644/-rw-r--r--)  Uid: ( 1000/  deploy)   Gid: ( 1000/  deploy)
Access: 2019-06-17 19:28:35.738783859 +0000
Modify: 2018-04-04 18:30:26.000000000 +0000
Change: 2019-06-16 09:07:40.356000000 +0000
 Birth: -


# To actually see the file creation time we have to use this clunky interface
# * 256077 is the inode number of the file we want
# * /dev/xvda1 is the block device of the disk
#
# The file "birth" time is "crtime" in the output
#
$ sudo debugfs -R 'stat <256077>' /dev/xvda1
Inode: 256077   Type: regular    Mode:  0644   Flags: 0x80000
Generation: 1071507318    Version: 0x00000000:00000001
User:  1000   Group:  1000   Project:     0   Size: 807
File ACL: 0
Links: 2   Blockcount: 8
Fragment:  Address: 0    Number: 0    Size: 0
 ctime: 0x5d07eb60:8238fa88 -- Mon Jun 17 19:34:56 2019
 atime: 0x5d07e9e3:b023c9cc -- Mon Jun 17 19:28:35 2019
 mtime: 0x5ac519c2:00000000 -- Wed Apr  4 18:30:26 2018
crtime: 0x5d0606dc:54e08400 -- Sun Jun 16 09:07:40 2019
Size of extra inode fields: 32
Inode checksum: 0x6a5adba4
EXTENTS:
(0):557126
```
