
# Show which shared libs are being loaded by a binary at runtime

Sources:

* http://www.bnikolic.co.uk/blog/linux-ld-debug.html

Overview

* You can get this info from `strace` on Linux but this is more efficient

```bash
# macos
DYLD_PRINT_LIBRARIES=1 ls
```

```bash
# linux

# LD_DEBUG has many options (help will show you them)
LD_DEBUG=help ls
```
