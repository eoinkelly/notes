# Raspberry Pi B+

## Types of pi

- Pi 1 model A+
    - smaller
    - cheaper
    - lower power than 1-A+
    - only 1 USB port
- Pi 1 model B+
    - 700 MHz single core ARMv6 CPU
    - 512 MB RAM
    - 40 ping GPIO
    - HDMI
    - 4 x USB ports
    - MicroSD card
    - can only run custom linux distros because most distros only support only
      arm7+
    - somewhat equivalent to the chip used in older smartphones (such as iPhone
      / 3G / 3GS).
    - Level 1 cache of 16 KB and a Level 2 cache of 128 KB. The Level 2 cache is
      used primarily by the GPU
    - SoC is stacked underneath the RAM chip, so only its edge is visible
    - the CPU level the performance is similar to a 300 MHz Pentium II of
      1997-1999
    - graphics capabilities of the Raspberry Pi are roughly equivalent to the
      level of performance of the Xbox of 2001
- Pi 2 model B
    - ARMv7
    - quad-core ARM Cortex-A7 CPU and a VideoCore IV dual-core GPU
    - 4–6 times more powerful than its predecessor. The GPU is identical
    - 900 MHz CPU
    - 1 GB RAM
    - otherwise similar to Pi 1 B+
    - ++ can run any linux distro or Windows 10
    - -- Higher power draw than its predecessor;
    - -- USB ports still limited to 1.2A in total;
    - -- Ethernet is still only 10/100
    - idle power draw for the Model B+ and Raspberry Pi 2 is similar, the
      additional three processing cores draw considerably more power (0.16A).

## Operating systems

- Pi 1
    - Raspbian (a Debian variant)
- Pi 2
    - Ubuntu Snappy Core
    - Raspbian (a Debian variant)
    - Pidora
    - RISC OS
    - media centre-based OSs (OpenELEC and OSMC)
    - Windows 10

```
# on a pi 1 model B+
$ uname -a
Linux raspberrypi 3.12.28+ #709 PREEMPT Mon Sep 8 15:28:00 BST 2014 armv6l GNU/Linux
```

Things to do with it

- Install go, elixir, yasm, vim, my dotfiles, rust
- Learn ARM assembler
- Do some ruby socket and thread programming
- see how peppy elixir is on it

# Acorn Risc Machine (ARM) Assembler

Sources:

    http://www.peter-cockerell.net/aalp/html/frames.html
    http://www.davespace.co.uk/arm/introduction-to-arm/why-learn.html
    http://www.toves.org/books/arm/

Arm 64 bit instruction set is very different to the 32 bit one

- Thumb
    - Thumb instructions are a 16-bit compressed form of the most commonly used
      ARM instructions.
        - key point: compressed
        - key point: subset
    - Instructions are (can be) dynamically decompressed in the instruction
      pipeline.
    - Thumb increases code density (commonly by 25%) at the cost of reduced
      execution speed.
        - I assume x86 has good code density given its many many specialised
          instructions
- Some ARM architectures can directly execute Java bytecodes
    - Java bytecodes are 8-bit instructions designed to be
      architecture-independent.

- Thumb-2
    - the progression of Thumb (strictly it is Thumb v3).
    - improves performance whilst keeping the code density tight by allowing a
      mixture of 16- and 32-bit instructions

- ARM is "load store"
    - you must load values into registers to operate on them
    - _No instructions operate directly on values in memory_

- all ARM registers are 32 bits wide
- AAPCS is the arm calling convention

```
Name    Alias (AAPCS)
R0      A1      (general purpose)
R1      A2      (general purpose)
R2      A3      (general purpose)
R3      A4      (general purpose)
R4      V1      (general purpose)
R5      V2      (general purpose)
R6      V3      (general purpose)
R7      V4  WR  (general purpose)
R8      V5      (general purpose)
R9      V6  SB  (general purpose)
R10     V7  SL  (general purpose)
R11     V8  FP  (general purpose)
R12     IP      (general purpose)
R13     SP      (Stack pointer)
R14     LR      (Link register (callers return address))
R15     PC      (Program counter)
```

CPSR (current program status register) \* ARM equivalent of RFLAGS

## AArch64

- pretty much a ground-up rewrite but backwards compatible with the backronym'd
  AArch32 32 bit architecture
- introducts a new A64 instruction set
- 31 general purpose 64bit registers
- program counter PC is no longer accessible as a register
- It adds a 64-bit architecture, named "AArch64", and a new "A64" instruction
  set.
- AArch64 provides user-space compatibility with ARMv7-A ISA
- the 32-bit architecture rebranded as "AArch32" and the old 32-bit instruction
  set, now named "A32".
- The Thumb instruction sets are referred to as "T32" and have no 64-bit
  counterpart.
- Apple A7 was first major chip released with this architecture
