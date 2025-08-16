Ext4 stores the following timestamps:

1. Access time (atime) when contents of file were last accessed
1. Modificaiton time (mtime) - when contents of file were last changed
1. Change time (ctime) - when the inode was last modified
1. Birth time - when the file was created (not well supported, waiting on kernel
   feature)

By default linux does not show you file creation (or "birth" time. Ext4 supports
it but `ls` and `stat` don't currently support reading it because they are
waiting to additions to the system calls. You can however read it for a single
file using a combination of `stat` and `debugfs`

```
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
# * 256077 is the inode number of the file we want (we got this from stat output above)
# * /dev/xvda1 is the block device of the disk (we got this from reading the output of `mount`)
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
