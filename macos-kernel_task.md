# Debugging why kernel_task is hogging the CPU

https://apple.stackexchange.com/questions/10738/how-do-i-debug-an-out-of-control-kernel-task-process

It could be drivers (kexts), network or disk activity.

https://apple.stackexchange.com/questions/178281/how-to-investigate-high-kernel-task-memory-usage/178353#178353

> Apparently when the CPU heats up the ACPI_SMC_PlatformPlugin.kext will take up
> CPU cycles in an attempt to reduce actual CPU load.

## Tool: kextstat

Examples:

```bash
# awk filters kext name and converts hex size to decimal
#
# -l stops printing of the header line which would confuse awk. The header line is:
#
# Index Refs Address Size Wired Name (Version) UUID <Linked Against>
#
# the -k omits built-in components of the kernel
kextstat -l -k | awk '{n = sprintf("%d", $4); print n, $6}' | sort -n


# show all non-Apple kernel extensions
kextstat | grep -v com.apple
```

Background

The kextstat utility displays the status of any kexts currently loaded in the
kernel.

Columns:

```
Index     The load index of the kext (used to track linkage references).  Gaps in the list indicate kexts that have been unloaded.

Refs      The number of references to this kext by others.  If nonzero, the kext cannot be unloaded.

Address   The address in kernel space where the kext has been loaded.

Size      The number of bytes of kernel memory that the kext occupies.  If this is zero, the kext is a built-in part of the kernel that has a record as a
        kext for resolving dependencies among kexts.

Wired     The number of wired bytes of kernel memory that the kext occupies.

Architecture (if the -a option is used)
        The architecture of the kext.

Name      The CFBundleIdentifier of the kext.

(Version)
        The CFBundleVersion of the kext.

<Linked Against>
        The index numbers of all other kexts that this kext has a reference to.
```

## Tool: fs_usage

```bash
sudo fs_usage | grep -v 0.0000

sudo fs_usage | grep -v -e '0.0000' -e 'iTerm2' -e 'grep'
```
