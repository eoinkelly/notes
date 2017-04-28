# What is ARM

* ARM = an architecture
    * includes being an instruction set
* Each version of the architecture/instruction set has
    * a particular set of registers
    * an assembly language

* there are three variants of the ARMv8 architecture (assembly language, registers, CPU architecture)
    1. A (Application) profile
        * high performance
    2. R (Real time) profile
        * embedded in automotive and industrial control
    2. M (Microcontroller) profile
        * embedded and IoT

* The ARMv8-A (architecture + profile)
    * doubles the number of registers of the A7 compared to the ARMv7 used in A6.
    * has 31 general purpose registers that are each 64-bits wide
    * has 32 floating-point/NEON registers that are each 128-bits wide


> ARM incorporates these typical RISC architecture features:
>
> A uniform register file load/store architecture, where data processing
> operates only on register contents, not directly on memory contents.
>
> Simple addressing modes, with all load/store addresses determined from register
> contents and instruction fields only.

NOTE

In ARMv8-A the Program counter (PC) and Stack pointer (SP) not general purpose registers
