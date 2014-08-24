# Unix Permissions

```
drwxr-xr-x    2 root       admin    68 24 Jun  2013 Typography
-rwxr-xr-x@   1 root       admin     0 24 Jun  2013 Windows 8 Receipt.pdf
```

## file type flag (first col):

Examples:

```
l = symbolic link
d = directory
- = regular file
s = socket
p = FIFO (pipe)
b = block specil file
c = character special files
(hard links show as whatever they link to)
```

Then 9 cols of permissions

the thrid col of each permission group can have values othe than `-` or `x`

```
s = set-user-ID or set-group-ID bit as well as executable bit is set
S = set-uid or set-groupid set but not executable
t = executable bit and restricted deletion falg are set
T = restricted deletion set but not executable
```

final col:

```
@ =  file has extended attributes. `ls -l@` to see them, `xattr` to edit
+ =  file has extended security information (e.g. ACL). `ls -le` to see ACLs
```

TODO: Find out more about extended attributes on mac
TODO: Find out more about mac ACLs
Q: How can I tell a hard-link from a real file?
