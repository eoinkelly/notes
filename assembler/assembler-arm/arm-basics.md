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


## Registers

A _user mode_ program on an ARM processor has access to 16 registers

* R0-R12 General purpose
* R13 Stack pointer
* R14 Link register
* R15 Program counter

There is a 17th register:

* Current Program Status Register (CPSR)
  * flags which you can read and set indirectly as part of other instructions

## 32 bit ARM assembly

* All ARM instructions are 32 bits long (even in 64 bit mode)

Data processing instructions have the following format

| idxs  | width   | description          |
| ----- | ------- | -------------------- |
| 0-11  | 12 bits | Immediate operand    |
| 12-15 | 4 bits  | Destination register |
| 16-19 | 4 bits  | Operand register     |
| 20    | 1 bit   | Set condition codes  |
| 21-24 | 4 bits  | Opcode               |
| 25-27 | 3 bits  | Operand type         |
| 28-31 | 4 bits  | Condition            |

* Condition
  * Allows the instruction to execute based on the bits in CPSR
* Operand type
  * tells you what the operands in bits 0-19 are
* Opcode
  * what instruction are we performing
  * 4 bits => 16 opcodes at most
* Set condition code
  * A single bit flag which says whether this instruction should update the CPSR
* Operand register
  * One register to use as input (4 bit field so max 16 addressable registers)
* Destination register
  * Where to put the result of this instruction (4 bit field so max 16 addressable registers)
* Immediate operand
  * lets you hard code operands into the instruction steam

Each instruction:

1. Takes one clock cycle to be loaded from memory
2. Takes one clock cycle to be decoded
3. Takes one clock cycle to execute

In each clock cycle the processor is moving instructions through the

    load -> decode -> execute

cycle

This works best with a linear block of instructions

## THe ugly truth about addressing main memory with 32 bit instructions

Notice that none of the operand boxes above are 32 bits so you can't directly put a whole memory address into it (e.g. when loading a value from memory)

The biggest field in the instruction is the 12 bit "immediate operand"

Question: So how do we address 32 bit values with only 12 bits available?
Answer: It treats the 12 bits as an offset from the current value of the PC (so you can load any value within 4096 words of the PC)
    * some of the bits are used as a shift to increase that 4096 range
      * HOW???

Happily assemblers take care of this drama for you. This does mean that what you read in an assembler listing will not match the assembler you write!