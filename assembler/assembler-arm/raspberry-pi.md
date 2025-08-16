# Raspberry Pi 3

Includes the `Broadcom BCM2837` SoC

> includes four high-performance ARM Cortex-A53 processing cores running at
> 1.2GHz with 32kB Level 1 and 512kB Level 2 cache memory, a VideoCore IV
> graphics processor, and is linked to a 1GB LPDDR2 memory module on the rear of
> the board.

- The Cortex-A53 supports the full ARMv8-A architecture.
- It not only runs 64-bit applications also seamlessly and efficiently runs
  legacy ARM 32-bit applications.

## My Raspberry Pi 3

- has 31 general purpose 64-bit registers
- has 32 floating point (NEON) registers which are 128bits wide

```
# specs for my Pi
Revision	Release Date	Model	    PCB Revision	Memory	Notes
a22082	    Q1 2016	        3 Model B	1.2	            1 GB	(Mfg by Embest)
```

```
pi@raspberrypi:~/asm $ cat /proc/cpuinfo
processor       : 0
model name      : ARMv7 Processor rev 4 (v7l)
BogoMIPS        : 38.40
Features        : half thumb fastmult vfp edsp neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm crc32
CPU implementer : 0x41
CPU architecture: 7
CPU variant     : 0x0
CPU part        : 0xd03
CPU revision    : 4

processor       : 1
model name      : ARMv7 Processor rev 4 (v7l)
BogoMIPS        : 38.40
Features        : half thumb fastmult vfp edsp neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm crc32
CPU implementer : 0x41
CPU architecture: 7
CPU variant     : 0x0
CPU part        : 0xd03
CPU revision    : 4

processor       : 2
model name      : ARMv7 Processor rev 4 (v7l)
BogoMIPS        : 38.40
Features        : half thumb fastmult vfp edsp neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm crc32
CPU implementer : 0x41
CPU architecture: 7
CPU variant     : 0x0
CPU part        : 0xd03
CPU revision    : 4

processor       : 3
model name      : ARMv7 Processor rev 4 (v7l)
BogoMIPS        : 38.40
Features        : half thumb fastmult vfp edsp neon vfpv3 tls vfpv4 idiva idivt vfpd32 lpae evtstrm crc32
CPU implementer : 0x41
CPU architecture: 7
CPU variant     : 0x0
CPU part        : 0xd03
CPU revision    : 4

Hardware        : BCM2835
Revision        : a22082
Serial          : 000000005319d64d

```

# Raspberry Pi 4

TODO
