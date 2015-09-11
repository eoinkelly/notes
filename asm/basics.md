# Assembler

# Sources

* Good blog:
    http://www.idryman.org/blog/2014/12/02/writing-64-bit-assembly-on-mac-os-x/
* Apple Mach-O file format:
    https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/MachORuntime/index.html
* OSX Assembler reference
    https://developer.apple.com/library/mac/documentation/DeveloperTools/Reference/Assembler/000-Introduction/introduction.html#//apple_ref/doc/uid/TP30000851-CH211-SW1
* Intel manuals http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html

# Dump info about my mac

My macbook CPU specs:

    Processor Name:   Intel Core i7
    Processor Speed:  1.7 GHz
    Number of Processors: 1
    Total Number of Cores:    2
    L2 Cache (per Core):  256 KB
    L3 Cache: 4 MB
    Memory:   8 GB

To see CPU details on command line do:

    sysctl -a | grep machdep.cpu


# Intel architectures and microarchitectures

Intel has 2 "architectures"

1. Intel 64
1. IA-32

* each architecture supports a different instruction-set

* and many many "microarchitectures" which vary based on the exact chip generation
* each "microarchitecture" goes with a family of processors
* microarchitectures have names like
    * haswell
    * sandy bridge
    * nehalem

It seems to be like Architecture is the category and microarchitecture is the
sub-category.

Things in common between the two architectures

* both are little endian
    * bytes in a word are numbered starting at least significant byte

# Intel manual notation

Intel manual uses

    label: memonic argument1, argument2, argument3
    label: mov dst src

to show instruction examples.

* Note order of source and destination!
* manual uses `H` suffix for hex numbers e.g. `FA3FH` is `0xFA3F`

# Assembler and assemblers

* When you write aseembly language in a file it is called "assembler"
* file extension is `.s`
* the program that turns your `.s` file into an object is also an assembler

There are two popular x86 assembler syntaxes

1. Intel syntax: `<instruction> <dst> <src1> <src2>`
    * dominates in windows world
    * used by:
        * NASM
            * Netwide Assembler (NASM)
            * available on mac
            * uses variation of intel syntax!!!
            * outputs lots of binary formats
            * installed by Xcode `NASM version 0.98.40 (Apple Computer, Inc.
              build 11) compiled on Jun 17 2015`
            * much newer version from homebrew `NASM version 2.11.08 compiled
              on Mar 10 2015` but have to invoke as `/usr/local/bin/nasm`
            * From nasm page:
            * > gas designed to be a back end to gcc, which always feeds it correct
              code. So its error checking is minimal. Also, its syntax is
              horrible, from the point of view of anyone trying to actually
              write anything in it.

        * DASM
            * Macro assembler with support for several 8-bit microprocessors
            * available on mac
        * FLASM
            * Flash command-line assembler and disassembler
            * available on mac
        * FASM
            * (flat assembler)
            * free, open source
            * outputs binaries in windows, linux formats
        * MASM
            * Microsoft Macro Assembler
        * TASM
            * Turbo Assembler
            * Windows only
1. Gnu syntax/GAS/AT&T Syntax: `<instruction> <src1> <src2> <dst>`
    * dominates in unix world

Assemblers that support both syntaxes

* YASM
    * Modular BSD reimplementation of NASM
    * available on mac
    * assembler and disassembler
    * supports both syntaxes
    * very modular
    * outputs lots of binary formats for mac, win, dos, linux
* GAS
    * appears as `as`
    * supports both syntaxes via the `.intel_syntax` directive
    * installed by Xcode `Apple Inc version cctools-870, GNU assembler version 1.38`
    * `/usr/bin/as` is a sort of front end for a bunch of assemblers that
      target different architectures there is a separate assembler for
        * 32-bit intel aka i386 aka IA-32
        * 64-bit intel aka x86_64 aka "Intel 64"

# Instruction memonics

* instruction memonic covers a family of instructions not just one

# Memory addressing

Intel processors support two kinds of memory addressing

1. byte addressing
    * simple, memory organised as a sequence of bytes
    * to read a byte you just provide the address
2. segment addressing
    * program has many independent address spaces e.g.
        * code (instructions)
        * stack
    * segment addresses have form `segment_register:byte_address_within_segment`
    ```
    DS:FF79H
    # segment given by contents of DS register
    # byte address within segment is 0xFF79
    ```

# Exceptions

* Processors can throw exceptions if they encounter an error.
* Notation is `#exception_memonic(fault_code)` e.g. `#PF(0)` for page-fault


From https://www.youtube.com/watch?v=H4Z0S9ZbC0g

Fundemental data types

* nibble = 4 bits
* byte/char = 8 bits
* word/short = 16 bit
    * from Intel POV a word is 16 bits because that was the word size on the
      processor they introduced words for.
* int/long/double word/dword = 32 bits
* long long/double/qword = 64 bits

Instructor recommend memorizing Dec <-> Binary <-> Hex conversions for dec 0 to 15


# 2's complement

Negative numbers are represented as the _two's complement_ of the corresponding
positive number.

    2's coplement = one's complement + 1
    one's complement = flip all the bits

    original:    0001b      0x1
    1's comp:    1110b      0xE
    2's comp:    1111b      0xF

    0x00 = 0
    0x01 = 1
    0x02 = 2
    0x03 = 3
    0x04 = 4
    ...
    0x7D = 125
    0x7E = 126
    0x7F = 127
    0x80 = -128
    0x81 = -127
    0x82 = -126
    ...
    0xFC = -4
    0xFD = -3
    0xFE = -2
    0xFF = -1

=> range is -128 to 127 for a single byte of storage

We arrange things so that we can tell whether a number is positive or negative
just by inspecting the high order bit - that is why there is one extra negative
value! See discussion of Sign Flag in EFLAGS below for an example of usage

=> you cannot represent d128 in a single byte in 2's complement

# CISC vs RISC

* CISC
    * many special purpose instructions that have been added based on patterns of usage observed in the wild
    * compilers don't use most of them
    * variable length instructions 1-16(ish) bytes max
    * Examples: Intel x86
* RISC Reduced instruction set computer
    * usually
        * more registers
        * less instructions
        * fixed-size instructions (fixed at the size of the architecture)
    * came from wanting a cleaner architecture
    * examples: PowerPC, ARM, SPARC, MIPS (note these are not all the same architecture)

# Endianness

* comes from gullivers travels - it doesn't matter which end you should break your hard-boiled egg at

* little endian
    * least significant byte stored first in memory
    * note that endian is about bytes only - it is NOT bits within a byte
    * intel stores thing in RAM as little endian but in registers is big endian - TRAP!
* big endian
    * Network traffic is big endian
    * english is big endian when we write numbers 123456 (the digit on left is the most significant)

# Registers

* small memory storage area built into the CPU
* will lose value if power is lost

There are

* 8 x general purpsuse registers
    * 2 of the 8 are not very general
* 1 x instruction pointer

* registers are 32 bit on x86-32
* registers are 64 bit on x86-64

Intel has "register conventions" - suggestions to compiler writers and
hand-coding assembly writing. You con't have to use the register conventions
but it makes it easier to understand.

1. EAX = stores function return values. Also used as an "accumulator" register.
    * functions that return a number will have a `mov` into it right before the function returns
2. EBX = Base pointer to the data section
3. ECX = Counter for string and loop operations
4. EDX = I/O pointer
5. ESI = source pointer for string operations
6. EDI = destionation pointer for string operations
7. ESP = stack pointer
8. EBP = stack frame base pointer
9. EIP = (Instruction Pointer) pointer to next instruction to execute

we can't move arbitrary values into IP but we can change it as a side effect of other insructions

Conventions for how functions argree to not destroy each others registers

Caller-save registers: EAX EDX ECX
    * If I am the caller and I have stuff I care about in any of these
      registers I know I need to save it somewhere before I do a function call
      as the callee can scribble over them.
    * If I am a callee I know I can do what I want with these registers
Callee-save registers: EBP EBX ESI EDI
    * If I am the callee and I need to use any of these registers I have to be
      careful to put everything back the way it was before I was called
    * If I am the caller I can trust that these registers will not be changed


You can grab sub-portions of registers (exists for backward compatability reasons)

You can grab single byte chunks of the The `*X` registers: EAX, EBX, ECX, EDX
You can grab the bits 0-16 of ESP, EBP, ESI, EDI, EIP

    QUESTION: can you grab bits 0-32 of RSP, RBP, RSI, RDI, RIP

    RAX = 64 bits
    EAX = bits 0-32 of RAX
    AX = bits 0-16 of EAX
    AH = bits 8-15 of EAX (single byte)
    AL = bits 0-7 of EAX (single byte)

    RSP = 64 bits
    ESP = bits 0-32 of RSP
    RAX = bits 0-16 of RSP

### EFLAGS register

The flags get set in here after _every_ instruction by the hardware - it is
handy to have a central place that these flags are set rather than checking for
results in the specific register.

EFLAGS is used a lot for conditional logic

* holds many single bit flags - each bit has a different name

* ZF (Zero Flag): set if the result of some instruction is zero; cleared otherwise
* SF (Sign Flag): Set equal the most significant bit of the result which is the sign bit of a signed integer
    * 0 => positive value
    * 1 => negative value

Hardware has no idea of signs of numbers - it does addition of bits and sets the flags appropriatly - it is up to the compiler to intrepret the result properly

# The Stack

A conceptional area of main memory which is _designated by the OS_ when a program is started
    * different OSes start it at different addresses by convention
* Stack is a LIFO/FILO data structure
* By convention on x86 the stack grows towards lower memory addresses
    * => "adding" to the stack means the top of the stack is now at a lower memory address
* ESP always points to the top of the stack - the lowest address which is being used.
* Things on the stack:
    * local variables
    * used to pass arguments to the next funtion to be called
* A good understanding of the stack is essential for understanding program operation

Calling conventions

How does function main pass parameters to a subroutine

How code calls a subroutine is compiler-dependent and configuralbe but there are some conventions:

* Caller cleanup (caller cleans up args off the stack)
    * examples
        * `cdecl` TODO: this is confusing
            * C declaration - most common calling convention
            * default for C code
            * function params pushed onto stack from right to left
            * issue the call instruction
            * save old stack frame pointer onto the stack and setup new stack frame
            * EAX or EDX:EAX returns the result for primitive data types
            * NB: caller is responsible for cleaning up the stack
* Callee cleanup( callee cleans up args off the stack)
    * allows for variable numbers of args
    * examples
        * `stdcall`
            * same as `cdecl` except the callee is responsible for cleaning up any stack params it takes
                * typically used in MS C++ code ("std" does not imply standard)

There are others: https://en.wikipedia.org/wiki/X86_calling_conventions

# Assembly Instructions

## NOP

* No operation, does nothing
* Just pad/align bytes or to delay time
    * Sometimes functions need to start on 16-byte boundaries
* Bad folks use it ot make buffer overflows more reliable :-)
* "Alias mnemonic for "XCHG EAX EAX"

## PUSH

* pushes a (word|double word|quad word) value onto the stack
* The _value_ can be an "immediate" value or a value in a register (any register except EIP)
    * immediate value is hard-coded into the assembly code
* When we push a DWORD onto the stack, then ESP (the stack pointer) needs to become (ESP - 4)

    push eax # pushes value of eax onto the stack

## POP

* pop a value from the stack into a register
* copies the value from the top of the stack to the give register and increments ESP to
* note that it doesn't delete or zero out the value from the stack area
