## ldd

- prints shared object dependencies
- ldd displays the location of the matching object and the (hexadecimal) address
  at which it is loaded
- `man ldd`

```
# to see what a binary is linked with
ldd  path/to/binary

$ ldd `which ls`
    linux-vdso.so.1 (0x00007fff7a7b0000)
    libselinux.so.1 => /lib/x86_64-linux-gnu/libselinux.so.1 (0x00007fa157dfd000)
    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fa157a0c000)
    libpcre.so.3 => /lib/x86_64-linux-gnu/libpcre.so.3 (0x00007fa15779a000)
    libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007fa157596000)
    /lib64/ld-linux-x86-64.so.2 (0x00007fa158247000)
    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007fa157377000)
```

### Security warning

- in some circumstances, some versions of ldd may attempt to obtain the
  dependency information by attempting to directly execute the program, which
  may lead to the execution of whatever code is defined in the program's ELF
  interpreter, and perhaps to execution of the program itself.
- You should never employ ldd on an untrusted executable, since this may result
  in the execution of arbitrary code.
- A safer alternative when dealing with untrusted executables is:

             $ objdump -p /path/to/program | grep NEEDED

- Note, however, that this alternative shows only the direct dependencies of the
  executable, while ldd shows the entire dependency tree of the executable
