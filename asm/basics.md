# x86-64 Assembler

### Sources

* [blog on writing 64 bit mac os assembler](http://www.idryman.org/blog/2014/12/02/writing-64-bit-assembly-on-mac-os-x/)
* [Apple Mach-O file format](https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/MachORuntime/index.html)
* [OSX Assembler reference](https://developer.apple.com/library/mac/documentation/DeveloperTools/Reference/Assembler/000-Introduction/introduction.html#//apple_ref/doc/uid/TP30000851-CH211-SW1)
* [Intel manuals](http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html)
* [Fantastic video series from OpenSecurityTraining](https://www.youtube.com/watch?v=H4Z0S9ZbC0g)

### Finding CPU info about a mac

    Apple menu > About This Mac > System report

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

    machdep.cpu.max_basic: 13
    machdep.cpu.max_ext: 2147483656
    machdep.cpu.vendor: GenuineIntel
    machdep.cpu.brand_string: Intel(R) Core(TM) i7-4650U CPU @ 1.70GHz
    machdep.cpu.family: 6
    machdep.cpu.model: 69
    machdep.cpu.extmodel: 4
    machdep.cpu.extfamily: 0
    machdep.cpu.stepping: 1
    machdep.cpu.feature_bits: 9221960262849657855
    machdep.cpu.leaf7_feature_bits: 12219
    machdep.cpu.extfeature_bits: 142473169152
    machdep.cpu.signature: 263761
    machdep.cpu.brand: 0
    machdep.cpu.features: FPU VME DE PSE TSC MSR PAE MCE CX8 APIC SEP MTRR PGE
    MCA CMOV PAT PSE36 CLFSH DS ACPI MMX FXSR SSE SSE2 SS HTT TM PBE SSE3
    PCLMULQDQ DTES64 MON DSCPL VMX SMX EST TM2 SSSE3 FMA CX16 TPR PDCM SSE4.1
    SSE4.2 x2APIC MOVBE POPCNT AES PCID XSAVE OSXSAVE SEGLIM64 TSCTMR AVX1.0
    RDRAND F16C
    machdep.cpu.leaf7_features: SMEP ERMS RDWRFSGS TSC_THREAD_OFFSET BMI1 HLE
    AVX2 BMI2 INVPCID RTM
    machdep.cpu.extfeatures: SYSCALL XD 1GBPAGE EM64T LAHF LZCNT RDTSCP TSCI
    machdep.cpu.logical_per_package: 16
    machdep.cpu.cores_per_package: 8
    machdep.cpu.microcode_version: 23
    machdep.cpu.processor_flag: 6
    machdep.cpu.mwait.linesize_min: 64
    machdep.cpu.mwait.linesize_max: 64
    machdep.cpu.mwait.extensions: 3
    machdep.cpu.mwait.sub_Cstates: 286531872
    machdep.cpu.thermal.sensor: 1
    machdep.cpu.thermal.dynamic_acceleration: 1
    machdep.cpu.thermal.invariant_APIC_timer: 1
    machdep.cpu.thermal.thresholds: 2
    machdep.cpu.thermal.ACNT_MCNT: 1
    machdep.cpu.thermal.core_power_limits: 1
    machdep.cpu.thermal.fine_grain_clock_mod: 1
    machdep.cpu.thermal.package_thermal_intr: 1
    machdep.cpu.thermal.hardware_feedback: 0
    machdep.cpu.thermal.energy_policy: 1
    machdep.cpu.xsave.extended_state: 7 832 832 0
    machdep.cpu.arch_perf.version: 3
    machdep.cpu.arch_perf.number: 4
    machdep.cpu.arch_perf.width: 48
    machdep.cpu.arch_perf.events_number: 7
    machdep.cpu.arch_perf.events: 0
    machdep.cpu.arch_perf.fixed_number: 3
    machdep.cpu.arch_perf.fixed_width: 48
    machdep.cpu.cache.linesize: 64
    machdep.cpu.cache.L2_associativity: 8
    machdep.cpu.cache.size: 256
    machdep.cpu.tlb.inst.large: 8
    machdep.cpu.tlb.data.small: 64
    machdep.cpu.tlb.data.small_level1: 64
    machdep.cpu.tlb.shared: 1024
    machdep.cpu.address_bits.physical: 39
    machdep.cpu.address_bits.virtual: 48
    machdep.cpu.core_count: 2
    machdep.cpu.thread_count: 4


## Intel architectures and microarchitectures

Intel has 2 "architectures"

1. Intel 64
2. IA-32

Each architecture supports a different instruction-set.

Intel has many "microarchitectures" which vary based on the chip generation.
Each "microarchitecture" goes with a family of processors. Microarchitectures
have names like

* haswell
* sandy bridge
* nehalem

It seems to be like Architecture is the category and microarchitecture is the
sub-category.

Things in common between the two architectures

* both store data bytes (not bits!) little endian
* bytes in a word are numbered starting at least significant byte

## Intel manual notation

Intel manual uses

    label: memonic argument1, argument2, argument3
    label: mov dst src

to show instruction examples.

* Note order of source and destination!
* manual uses `H` suffix for hex numbers e.g. `FA3FH` is `0xFA3F`

## Assembler and assemblers

* When you write aseembly language in a file it is called "assembler"
* file extension is `.s` or `.asm`
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
2. Gnu syntax/GAS/AT&T Syntax: `<instruction> <src1> <src2> <dst>`
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

## Disassemblers

* GUI for mac
    * IDA - seems to be the Photoshop of disassemblers with a photoshop like
      price to go with it.
    * Hopper
        * seems good
        * cheaper than IDA
        * free demo quite crippled
* Command line on Mac
    * `nm <binary>` - displays symbol table
    * `otool -t <binary>` show contents of `__TEXT` section as hex
    * `otool -vt <binary>` show contents of `__TEXT` and disassemble it (ATT syntax)
    * `otool -Vt <binary>` show contents of `__TEXT` and symbolically
      disassemble it (ATT syntax)
    * `/usr/local/bin/ndisasm -p intel -b 64 -a <binary>`
        * dumps much more than just the TEXT section
* Command line on Linux
    * ???

## Processor execution modes

1. real mode
    * 16 bit
        * ??? 16 bits of what?
    * segmented memory addresses (can address 1 MB via 20bit addresses)
    * direct software access to peripheral hardware
    * no concept of memory protection or multitasking at the hardware level
2. protected mode
    * 16 and 32 bit
    * 16 MB of addressable physical memory
    * 1 GB addressable virtual memory
    * provides protected memory
        * segment registers do not store real segment offsets - they store an
          index into a "descriptor table"
        * two such tables
            * global descriptor table
            * local descriptor table
        * each memory segment can be assigned one of 4 "ring levels" which
          allows for some hardware security
3. long mode
    * 64 bit
4. virtual 86 mode
    * 16 bit
5. system management mode
    * 16 bit

The processor runs in real mode immediately after power on. The OS has to
explicitly change mode

### Aside: IA-64 is not x86-64

* IA-64 (Itanium architecture) is a different architecture to x86-64
    * created by Intel
    * has different instruction set to IA-32
    * was hoped to replace x86 architecture but didn't take off
* x86-64 is a 64 bit extension to the x86 architecture
    * AMD created x86-64 as extensions to x86-32
    * much less radical approach, allowed better interop with existing code
    * Intel was forced to implement x86-64, branded it "Intel 64"

IA-64 is mostly of historial interest now.

## Memory addressing

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
        The calculation for converting a segment address into a byte address is
        ```
        (segment_address * 0x10) + offset
        # 0x10 is decimal 16 so the multiplication is a 4 bit shift right
        ```

The segment:offset format lets us have 20 bit addresses
    => you can address up to 1MB of RAM

QUESTION: what processor modes does this apply to?

## Exceptions

* Processors can throw exceptions if they encounter an error.
* Notation is `#exception_memonic(fault_code)` e.g. `#PF(0)` for page-fault
* Examples:
    * page fault
    * an attempt to access kernel memory by a process

# Fundemental data sizes

* nibble = 4 bits
* byte/char = 8 bits
* word/short = 16 bit
    * a word is 16 bits because that was the word size on the processor they
      first introduced words on.
* int/long/double word/dword = 32 bits
* long long/double/qword = 64 bits

Instructor recommend memorizing Dec <-> Binary <-> Hex conversions for dec 0 to 15

# 2's complement representation

Negative numbers are represented as the _two's complement_ of the corresponding
positive number.

```
2's coplement = one's complement + 1
one's complement = flip all the bits
```

```plain
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
```

* => range is -128 to 127 for a single byte of storage
* => you cannot represent d128 in a single byte in 2's complement

We arrange things so that we can tell whether a number is positive or negative
just by inspecting the high order bit - that is why there is one extra negative
value! See discussion of Sign Flag in EFLAGS below for an example of usage

### Aside: CISC vs RISC

* CISC
    * many special purpose instructions that have been added based on patterns
      of usage observed in the wild
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

### Endianness

* comes from gullivers travels - it doesn't matter which end you should break
  your hard-boiled egg at.

* little endian
    * least significant _byte_ stored first in memory.
    * note that endian is about bytes only - it is NOT bits within a byte
    * intel stores thing in RAM as little endian but in registers is big endian - TRAP!
    * bytes are numbered from least significant to most e.g. byte 0 of a
      quad-word is the least significant byte of an 8 byte value.
* big endian
    * Network traffic is big endian
    * english is big endian when we write numbers 123456 (the digit on left is
      the most significant)

## Registers

* small memory storage area built into the CPU
* will lose value if power is lost

There are

* 1 x instruction pointer register `rip`
* 16 x general purpose registers
* 16 floating point registers
* other special registers for SSE etc.

Intel has "register conventions" - suggestions to compiler writers and
hand-coding assembly writing. You con't have to use the register conventions
but it makes it easier to understand.

1. RAX
	* stores function return values. Also used as an "accumulator" register.
    * functions that return a number will have a `mov` into it right before the
      function returns
2. RBX
	* Base pointer to the data section
3. RCX
	* Counter for string and loop operations
4. RDX
	* I/O pointer
5. RSI
	* source pointer for string operations
6. RDI
	* destionation pointer for string operations
7. RSP
	* stack pointer
8. RBP
	* stack frame base pointer
9. RIP
	* (Instruction Pointer) pointer to next instruction to execute
10. R8
11. R9
12. R10
13. R11
14. R12
15. R13
16. R14
17. R15
18. RFLAGS
    * a collection of 1 bit flags

Recommended use of the general purpose registers:

* AL/AH/AX/EAX/RAX: Accumulator
* BL/BH/BX/EBX/RBX: Base index (for use with arrays)
* CL/CH/CX/ECX/RCX: Counter (for use with loops and strings)
* DL/DH/DX/EDX/RDX: Extend the precision of the accumulator (e.g. combine 32-bit EAX and EDX for 64-bit integer operations in 32-bit code)
* SI/ESI/RSI: Source index for string operations.
* DI/EDI/RDI: Destination index for string operations.
* SP/ESP/RSP: Stack pointer for top address of the stack.
* BP/EBP/RBP: Stack base pointer for holding the address of the current stack frame.
* IP/EIP/RIP: Instruction pointer. Holds the program counter, the current instruction address.

We can't move arbitrary values into RIP but we can change it as a side effect
of other insructions.

You can grab sub-portions of registers (exists for backward compatability reasons)

How register names are nested:

    [RIP  [EIP  [IP]]]
    [RAX  [EAX  [AX  [AH][AL]]]]
    [RBX  [EBX  [BX  [BH][BL]]]]
    [RCX  [ECX  [CX  [CH][CL]]]]
    [RDX  [EDX  [DX  [DH][DL]]]]
    [R10  [R10D [R10W  [R10L]]]]
    in progress...

You can grab single byte chunks of the The `*X` registers: EAX, EBX, ECX, EDX

You can grab the bits 0-16 of ESP, EBP, ESI, EDI, EIP

    QUESTION: can you grab bits 0-32 of RSP, RBP, RSI, RDI, RIP

    RAX = 64 bits
    EAX = bits 0-32 of RAX
    AX = bits 0-16 of EAX
    AH = bits 8-15 of EAX (single byte)
    AL = bits 0-7 of EAX (single byte)


You can use any general purpose register for any task _but_ hardware will be
faster if you follow the guides e.g. RAX will be fastest to use as an
accumulator

### Segment registers

* not general purpose
* cannot be set or read from directly ???
* each segment register determines where a particular 64KB segment starts

Segment registers:

    CS: Code segment
    DS: Data segment
    SS: Stack segment
    ES: Extra data segment
    FS: Extra data #2 segment
    GS: Extra data #3 segment

### SIMD registers

There are other "extension" registers for specialized tasks e.g.

* MMX
    * MMX is an instruction set!
    * MMX doesn't stand for anything - is a marketing term from Intel
    * it is a single instruction, multiple data (SIMD) instruction set
    * defines 8 registers MM0 to MM7
    * are aliases to the x87 floating point registers
        * hardware takes care of making their contents always be same
    * are directly addressable (unlike the FP registers)
* 3DNow!
    * AMD expansion on MMX instruction set
    * Never popular, dropped since 2010
* SSE, SSE2, SSE3, SSE4
    * streaming SIMD instructions
    * a further expansion of MMX instruction set
    * is what modern processors have

### RFLAGS register

The flags get set in here after _every_ instruction by the hardware - it is
handy to have a central place that these flags are set rather than checking for
results in the specific register.

RFLAGS is used a lot for conditional logic

* holds many single bit flags - each bit has a different name

* AF Auxilliary flag
    * used for conditional logic
* CF Carry flag
    * used for conditional logic
* OF Overflow flag
    * used for conditional logic
* PF Parity flag
    * used for conditional logic
* SF (Sign Flag):
    * Set equal the most significant bit of the result which is the sign bit of
      a signed integer
        * 0 => positive value
        * 1 => negative value
* ZF (Zero Flag)
    * set if the result of some instruction is zero; cleared otherwise

Hardware has no idea of signs of numbers - it does addition of bits and sets
the flags appropriately - it is up to the compiler/programmer to intrepret the
result properly.

    TODO: understand that statment properly

## The Stack

* A conceptional area of main memory which is _designated by the OS_ when a
  program is started.
    * different OSes start it at different addresses by convention
* Stack is a LIFO/FILO data structure
* By convention on x86 the stack grows towards lower memory addresses
    * => "adding" to the stack means the top of the stack is now at a lower memory address
* RSP always points to the top of the stack - the lowest address which is being used.
    * pop off the stack => RSP = RSP + sizeof(thing_you_popped)
    * push onto the stack => RSP = RSP - sizeof(thing_you_pushed)
* Things on the stack:
    * local variables
    * used to pass arguments to the next funtion to be called
    * addresses of other parts of the stack - the stack frames form a linked list
* A good understanding of the stack is essential for understanding program operation
* Stack frame pointers create a linked list

* stack grows down from very top of logical address space
    * "to reserve space on the stack" == subtract num bytes required for all local vars from ESP
    * Add to rsp => remove bytes from top of stack
    * Subtract from rsp => add more bytes to top of stack

## Function call conventions

Compilers have conventions about the preparation that happens before a function
call and the cleanup that happens afterwards.

1. the caller does some "caller saves" to save off the values of any registers that the callee might overwrite
2. The caller pushes the local arguments for the callee onto the stack (from right to left parameter)
    * pushing on from right-to-left means the callee will read them off from left-to-right!
3. execute the CALL instruction (see above)
1. callee takes callers frame pointer from EBP and puts it on stack for later
1. writes into EBP the new top of the stack as that will be the start of callee's stack
1. callee saves its callee-save registers onto the stack
1. callee reserves space for its own local variables on the stack

* You can tweak the above process with compile options e.g. `-fomit-frame-pointers` in GCC
* The above convention is what compilers will do - you don't have to do exactly
  this when you hand-write assembly.
### Register clean-up

Conventions for how functions argree to not destroy each others registers

* Caller-save registers: RAX RDX RCX
    * If I am the caller and I have stuff I care about in any of these
      registers I know I need to save it somewhere before I do a function call
      as the callee can scribble over them.
    * If I am a callee I know I can do what I want with these registers
* Callee-save registers: RBP RBX RSI RDI
    * If I am the callee and I need to use any of these registers I have to be
      careful to put everything back the way it was before I was called
    * If I am the caller I can trust that these registers will not be changed

### Pushing args onto the stack and popping them off after

How code calls a subroutine is compiler-dependent and configurable but there
are some conventions for 32 bit code:

* Caller cleanup (caller cleans up args off the stack)
    * examples
        * `cdecl`
            * C declaration - most common calling convention
            * default for C code
            * the process:
                1. caller pushes function params onto stack from right to left
                1. caller issues CALL instruction
                    * pushes next instruction after call onto stack (for RET to use)
                    * changes RIP to point to new instruction
                1. callee saves current RBP (stack frame pointer) onto the stack
                1. callee copies current stack pionter into the RBP so current
                   top of stack becomes the start of a new stack frame.
                1. callee ... executes its instructions ...
                1. callee puts its return value in EAX or EDX:EAX
                1. callee issues RET which pops the top of stack into RIP
                    * how does it know where to find that value? is it (RBP + 4)?
                1. caller cleans up stack by popping the args it put on there off.
                    * how???
            * NB: caller is responsible for cleaning up the stack
    * caller cleanup allows for variable numbers of args.
* Callee cleanup( callee cleans up args off the stack)
    * cannot do variable numbers of arguments as the exact no. of bytes to be
      wiped off the stack have to be known at compile time
    * examples:
        * `stdcall`
            * same as `cdecl` except the callee is responsible for cleaning up
              any stack params it takes
                * typically used in MS C++ code ("std" does not imply standard)

Note: there is no difference between cdecl and stdcall unless the callee gets arguments

There are [other calling conventions](https://en.wikipedia.org/wiki/X86_calling_conventions)

The calling convention is different in x86-64 because there is a lot more
register space available.

> The calling convention of the System V AMD64 ABI[14] is followed on Solaris,
> Linux, FreeBSD, Mac OS X, and other UNIX-like or POSIX-compliant operating
> systems. The first six integer or pointer arguments are passed in registers
> RDI, RSI, RDX, RCX, R8, and R9, while XMM0, XMM1, XMM2, XMM3, XMM4, XMM5,
> XMM6 and XMM7 are used for floating point arguments. For system calls, R10 is
> used instead of RCX. As in the Microsoft x64 calling convention, additional
> arguments are passed on the stack and the return value is stored in RAX.

# R/MI Form

* `r/mi32` is a phrase in the intel manuals
* it refers to something which could be a register or memory
* In intel synax `[]` always means "from memory" except in the `LEA` instruction
* `[]` is assembly syntax for dereferencing a pointer

```assembly
    # MOV DST, SRC
    MOV eax, ebx            # move ebx into eax
    mov eax, [ebx]          # move memory pointed at by ebx into eax
    mov eax, [ebx+ecx*X]    # move memory pointed at by the result of the expression into eax

    mov eax, [ebx + ecx*X + Y]
    # Most general form: [base + index*scale + displacement]
    # X element of {1,2,4,8}
    # Y is a constant offset (called a displacement) - can be 1 byte or 4 bytes
```

Given an array of 64bit unsigned integers in memory and

    rax = address of the start of an array

how do we find the memory addresses of elements of the array?

To get the 4th array element (index 3) we:

    mov rcx, 3
    mov r8, [rax + rcx*8]

X is 8 here because the array elements are 8 bytes

The displacement (Y) can help with:

* In multi-dimensional arrays you could set Y to whatever the distance of the
  inner array from the start of the outer array.
* if you have contigious arrays and want to displace past the first array to
  another one.
* An array of structures is fundementally a multi-dimensional array so
  displacement is useful there too

# Assembler Instructions

* instruction memonic covers a family of instructions not just one i.e. there
  are many ways the `MOV` instruction can be encoded as binary in the
  instruction steam.

## NOP

* No operation, does nothing
* Just pad/align bytes or to delay time
    * Sometimes functions need to start on 16-byte boundaries
* Bad folks use it ot make buffer overflows more reliable :-)
* "Alias mnemonic for "XCHG EAX EAX"

## PUSH

* pushes a (word|double word|quad word) value onto the stack
* that is:
    1. copies the value onto the stack
    2. adjusts RSP to point to the new top of the stack
* The _value_ can be an "immediate" value or a value in a register (any register except RIP)
    * immediate value is hard-coded into the assembly code
* When we push a DWORD onto the stack, then RSP (the stack pointer) needs to become (RSP - 4)
    ```
    push rax ; pushes value of rax onto the stack & adjust rsp
    ```

## POP

* pop a value from the stack into a register
* that is:
    1. copies the value from top of the stack
    2. adjusts RSP to point to the new top of the stack
* copies the value from the top of the stack to the given register and
  increments ESP to
* note that it doesn't delete or zero out the value from the stack area

    TODO: You can apparently pop directly into a memory address but instructor
    doesn't go into it.

## CALL

* transfer control to a different function in a way that control can be
  transfered back later
    ```
    CALL <address>
    ```

What it does:

1. push address of next instruction on to the stack
    * RET will take this off the stack and put it in RIP when the function returns
    * side effect: CALL modifies the stack
2. change RIP to point to `<address>` given as arg to CALL
    * CALL implicitly modifies RIP
    * address can be
        1. absolute
        2. relative to the end of the instruction

In the absence of CALL the CPU just works its way through the instructions
sequentially - it keeps incrementing the RIP

## RET

Has 2 forms

1. `RET`
    * POP the top of the stack into RIP (POP increments the stack pointer)
    * RET is an implicit POP into RIP
2. `RET 0x<some-hex-value>`
    * POP the top of the stack into RIP and move the stack pointer to clean up
      the parameters it got i.e. add a constant no. of bytes to ESP
        * Example: if it got 3 params that were 64bit ints then it would add 12
          to the stack pointer ESP to clean up the stack. Remember adding to
          ESP actually moves ESP _down_ the stack.
    * typically used by stdcall functions

Some disassemblers will use the `RETN` mnemoic for this instruction

## MOV

Can move

    # Intel syntax:
    MOV DST, SRC

    # ATT syntax:
    MOV SRC, DST
* memory -> register
* register -> memory
* immediate -> register
* immediate -> memory
* it cannot move memory -> memory !!
    * intel generally does not give you instructions that will do memory to
      memory operations e.g. addition
* Note the destination is first operand - intel asm data travels right to left
  (west going)

Memory addresses are given in "r/m32 form"

An immedate is a constant that is hard-coded into the instruction stream

## ADD and SUB

    ADD DST, SRC
    SUB DST, SRC

* source can be
    * register
    * memory
    * immediate
* dst can be
    * register
    * memory
* source and dst cannot both be memory (x86 has no memory to memory transfer)
* Note the destination is first operand - intel asm data travels right to left
  (west going)
* evaluates the operation (addition or subtraction) as if it were operating on
  both signed and unsigned data - it sets EFLAGS appropriately to indicate
* modifies
    * AF auxilliar flag
    * CF carry flag
    * OF overflow flag
    * PF
    * SF
    * ZF

    ADD esp, 8
    SUB eax, [ebx*2]

## LEA

* Load effective address
* Is the one instruction where `[]` does not mean "dereference" ("get the value at")
* Used for both general arithmetic and pointer arithmetic
    * it cannot do arbitrary arithmetics but it can do addtions and
      multiplications by 2,4,8 - see discussion of r/mi form above.
* Is kind of a subset of MOV
    * Does the calculation within the `[]` but does not load the memory
    ```
    LEA eax, [edx + ebx*2]
    ```
* Note the destination is first operand - intel asm data travels right to left
  (west going)
* Uses
    * You want to calculate the address of the Nth element of an array but you
      don't want to load the data at that address.

## SHL

* logical shift left
    ```
    SHL esi 0x1
    # shift esi left by 1 bit
    ```

## JMP

* change RIP to given address
* has many forms
    * short relative
        * 1 byte displacement from end of JMP instruction
        * can jump -128 to 127 instructions around the end of JMP itself
    * near relative
        * 4 byte displacment from end of JMP instruction
    * absolute
        * hardcoded address in instruction
        * library code can be moved around so you probably shouldn't use this.
    * absolute indirect
        * jumping to an absolute address but that address is not stored in the instruction
        * address calculated with r/m32
        * CALL uses absolute indirect for function pointers
* C `goto` is basically `jmp`
* Displacement is relative to the end of the JMP instruction
    * `JMP -2` is a 2 byte instruction so will make an infinite loop.

In disassembly jmps tend to look absolute even if they are not - the
disassembler is trying to help you by showing you the addres sit will jump to
but if the jump is near to the start point the compiler might have emited a
short relative or near relative JMP.

## JCC family of instructions (jump if condition is met)

* there are 4+ pages of jump types (some are synonyms)
* JNE == JNZ (synonyms)
    * JNE: Jump if not equal
    * JNZ: Jump if not zero flag set
    * both check the Zero Flag (ZF == 0)

* JE, JZ:       JMP if ZF == 1
* JNE, JNZ:     JMP if ZF == 0
* JLE, JLG:     JMP if ZF == 1 or SF != OF
* JGE, JNL:     JMP if SF == OF
* JBE:          JMP if CF == 1 or ZF == 1
* JB:           JMP if CF == 1

Instructor recommends not memorizing these flag values - can look it up in a table if you really care but mostly you won't

OF = overflow flag
SF = sign flag

The zero flag is set if the result of an operation is zero
The zero flag is stays set until some other instruction changes it (it has no "timeout")

## CMP

* behind the scenes is actually a subtract instruction
    ```
    SUB a, b
    ```
compute b - a and set flags the same way SUB would and discard ther esult
    => it won't mess up your registers
* modifies SF, OF, CF, ZF, AF, PF
* is different from SUB because it discards the result
* sets condition flags for you
* they do a computation, throw away the results but set the flags

## TEST

```
TEST A B
```

* similar to CMP it exists just to set flags
* does a bitwise logical AND of A and B and sets flags (result is discarded
* sets SF, ZF, PF flag

### Aside: calling C main function

Things that happen when entering main

Main gets 2 args arc, argv

    int main(argc, char ** argv) {

1. pre-main: push argv onto stack
2. pre-main: push argc onto stack
3. pre-main: do `CALL main` which will
    1. push the address of the instruction after the call onto the stack
    2. modify rip to point at the first instruction in `main`
4. main: push rbp onto stack (rbp holds the address of the start of the previous
         stack frame)
5. main: copy rsp into rbp - this effectively makes the current top of stack the
         new frame pointer. All insturctions after this consider themselves to
         be in the new "frame"
6. main: execute the other instructions

## How instructions are encoded in the instructions stream

Instructions e.g. `MOV` have many possible encodings in the instruction stream.

* MOV can be a 3 byte or 4 byte instruction depending on what it is doing

instruction # encoding
push rbp    # 55

## C features and the assembly they generate

* Unconditional control flow:
    * examples:
        * procedure calls
        * goto
        * exceptions
        * interrupts

* Conditional control flow
    * examples:
        * if/else
        * switch
        * for, while loops

Conditional logic uses RFLAGS register

[IN PROGRESS -
VIDEO 1,2,3 DONE,
4 up to 52:00
]

# Book: Introduction to 64 bit Assembly Language Programming for Linux

## Significant features of 64 bit architecture

* In 64 bit architecture segment registers are "essentially obsolete" because
  of the extra register size.
* more register usage is general purpose
    * except "repeat-string" loops which use specific registers and have no
      operands
* 16 general purpose registers
* 16 floating point registers

Pages in memory begin with addresses that the 3 right most bits are 0

    0x???? ???? ???? ?000

## Evolution of programming languages

* 1st gen language: Machine language
    * enter all intructions and bytes as hex data
    * you have to manually calculate and know the addresses of your data
    * the destination addresses of your JMPs will change as you add/delete
      source code so all those calculations would change every time. Bummer.
        * I wonder what features of modern computer architecture are a result
          of that constraint? e.g. starting stack at top and growing down ???
* 2nd gen language: Assembly languages
    * lets you use symbols to refer to memory addresses so you don't have to
      change everything when you add some data.
* 3rd gen language: Cobol & Fortran

## Why bother with assembler?

* all variable access is pointer access - safety schmafety.
* assembly is approx 2x as slow even for experts
* great way to learn about computer constraints

## Assembler instructions

Instructions are one of

1. machine instructions
    * exaples: `mov`, `leq` etc.
2. macros
    * examples: ???
3. instructions to the assembler
    * called a "pseudo opcode" or "assembler directive"
    * examples
        * `segment .<segment-name>`
        * `globl <symbol-name>` (note spelling)
            * tells the assembler to make the given symbol visible to the linker.


* `_start` is the entry point for all linux programs

### Floating point representation in 64 bit architectures

64 bit architecture supports 3 kinds of floating point number

* float = 32 bits
* double = 64 bits
* long double = 80 bits

Floating point representation is a sort of "storage format" for a number -
floats are converted to a standard binary representation before being operated
on and the results converted back afterwards

### How processor converts logical address to real physical address

* x86-64 CPUs have "hardware mapping registers"
* which can map different page sizes:
    * 4 KB (used by linux kernel for most things)
    * 2 MB (used by linux for kernel)
    * 1 GB (only in recent CPUs)

A process's POV of memory is 4GB of "logical addresses"
Maximum length of a logical address is 48 bits not 64

    logical-address = 36 bit page-number + 12 bit offset-within-page

Take example of a 4 KB page at logical address `0x40_0000_2220`

1. interpret the logical address
    * we need 4096 "offsets" so the right-most 12 bits is the offset `0x220`
    * the rest of the bits are the "page number" `0x40_0000_2`

2. ask the hardware mapping registers to convert page number into a physical
   page offset

The OS has special instructions for managing the hardware mapping registers

# Linux memory areas

Memory is divided into 4 areas

1. Stack
    * always mapped to the highest address of a process
        * highest valid stack address: `0x7fff_ffff_ffff` (131 TB)
        * lowest valid stack address:  `0x7fff_ff00_0000` (stack grows down)
        * Based on the 48 bit logical addresses
        * restricted to 16MB by the kernel
            * => stack space is a constrained resource
        * the stack will automatically grow when needed by the OS responding to
          a page fault.
1. Heap
    * the heap is not implemented with a "Heap" data structure!
    * a dynanically allocoated chunk of memory
    * can grow as much as physical memory and swap will allow
1. Data
    * placed right above the text segment
    * contains both
        * .data
            * data statically allocated *and* initialized by the compiler
        * .bss
            * data statically allocated by the process but not stored in the executable
            * initialized to all `0` when process is launched
1. Text
    * the instruction stream lives here
    * the lowest address is `0x0000_0040_0000`

## Inspecting a process address space on mac

    vmmap <PID>

## Inspecting a process address space on linux

```
cat /proc/<PID>/maps

# $$ = current process
cat /proc/$$/maps
```

Columns in /proc/PID/maps output:

* start_address
* end_address
* permissions
    * r = read
    * w = write
    * x = execute
    * p = private, s = shared
* offset
    * if the region was mapped from a file this is the offset in the file that mapping begins
    * is 0 if region was not mapped from file
* device
    * major:minor device number if the region was mapped from file
* inode
    * inode number of file if region was mapped from file
* pathname
    * full path to the file if the region was mapped from file
    * special "paths"
    * `[heap]`
    * `[stack]`
    * `[vsyscall]`
    * `[vdso]`
        * virtual dynamic shared object
        * used by system calls to switch to kernel mode

[more info](http://stackoverflow.com/questions/1401359/understanding-linux-proc-id-maps)

## Address Space Layout Randomization ASLR

Address space layout randomization ASLR is a process of using a _somewhat_
random stack, data, heap start addresses.

* makes buffer overflow and stack overflow attacks harder
* linux has it since 2005
* OS X since Lion
* iOS, Android, FreeBSD also have it
* Windows has it since vista and had a good one since Windows 8

```
# can disable address space randomisation via:
sudo sh -c 'echo 0 > /proc/sys/kernel/randomize_va_space'
```

## Op-codes for reserving data in the `.data` section

* db = reserve and initialize byte (8 bits)
* dw = reserve and initialize word (16 bits)
* dd = reserve and initialize double word (32 bits)
* dq = reserve and initialize quad word (64 bits)

```asm
section .data
    ; <var-name> <op-code> <value>
    a dd 4              ; create a 4 byte int
    b dd 4.4            ; create a 4 byte float
    c dw 1, 2, 3        ; create an array of 3 elements
    d db 0xfe           ; create a byte
    e db "hello", 0     ; create a byte array & intialize with a string. Note the explicit null termination

    ; there is a shortcut for bulk initialization
    ;<var-name> times <multiplier> <reservation-opcode> <initial-value>
    c times 10 dd 0
```

## Op-codes for reserving space in the `bss` section

* resb = reserve byte
* resw = reserve word (16 bits)
* resd = reserve double word (32 bits)
* resq = reserve quad word (64 bits)

```asm
section .bss
    ;<var-name> <op-code> <multipler>
    a resd 1    ; reserve 1 double-word (4 byte) area
    b resw 10   ; reserver 10 * 16bit areas
    c resb 100  ; reserve 100 single byte areas
```

UP TO PAGE 70

# Aside: How C stores arrays

This C file

```c
// array_test.c
#include <stdio.h>

char everywhere[3] = {'a', 'b', 'c'};

int main() {
    int arr[3] = {1, 2, 3};
    printf("%d", arr[2]);
}
```

generates the following assembler via `gcc -S array_test.c`

```asm
	.file	"array_test.c"

    ; ;;;;;;;;;;;;;;;;;;;;
    ; here we put the 3 element array into the data segment
    ; note that GCC writes out a type and a size for the array too
    ; ;;;;;;;;;;;;;;;;;;;;
	.globl	everywhere
	.data
	.type	everywhere, @object
	.size	everywhere, 3
everywhere:
	.byte	97
	.byte	98
	.byte	99


	.section	.rodata
.LC0:
	.string	"%d"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6


    ; ;;;;;;;;;;;;;;;;;;;;
    ; here we put the 3 element array on the stack
    ; ;;;;;;;;;;;;;;;;;;;;
	subq	$16, %rsp
	movl	$1, -16(%rbp)
	movl	$2, -12(%rbp)
	movl	$3, -8(%rbp)


	movl	-8(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 4.8.4-2ubuntu1~14.04) 4.8.4"
	.section	.note.GNU-stack,"",@progbits
```

