# Book: Introduction to 64 bit Assembly Language Programming for Linux

## Introduction

Terminology

* Assembler vs Assembly
    * assembly is to assembler as c is to gcc
    * Assembly is the language you put in the text file. An assembler turns that into an object file
* macro assembler
    * a tool that allows paramaterized assembly language text can be represented by a name and the expanded version inserted into the code
    * most modern assemblers have preprocessors e.g `yasm` has a selection of them: nasm, tasm, raw, cpp, gas
    * ASSUMPTION: they function similarly to the C preprocessor.
        * the `cpp` preprocessor for `yasm` actually uses a C preprocessor
* high-level assembler
    * provides things like if/else, loops etc.
* cross assembler
    * allows you to build object files for platform X on platform Y
* micro assembler
    * builds firmware
* meta assembler
    * a program that given a description of an assembly language can generate and assembler for it

ASIDE: The attitude of assembly language programmers is that they do it because they must but want to keep as many nice things as they would have if they used something like C ("nice things" is relative!)

### Significant features of 64 bit architecture

* In 64 bit architecture segment registers are "essentially obsolete" because
  of the extra register size.
* more register usage is general purpose
    * except "repeat-string" loops which use specific registers and have no
      operands
* 16 general purpose registers
* 16 floating point registers

Pages in memory begin with addresses that the 3 right most bits are 0

    0x???? ???? ???? ?000

### Evolution of programming languages

* 1st gen language: Machine language
    * enter all intructions and bytes as hex data
    * you have to manually calculate and know the addresses of your data
    * the destination addresses of your JMPs will change as you add/delete
      source code so all those calculations would change every time. Bummer.
        * I wonder what features of modern computer architecture are a result
          of that constraint? e.g. starting stack at top and growing down?
* 2nd gen language: Assembly languages
    * lets you use symbols to refer to memory addresses so you don't have to
      change everything when you add some data.
* 3rd gen language: Cobol & Fortran

### Why bother with assembler?

* all variable access is pointer access - safety schmafety.
* writing assembly is approx 2x as slow even for experts
* great way to learn about computer constraints

> increasing processor performance has meant that most CPUs sit idle most of
> the time, with delays caused by predictable bottlenecks such as cache
> misses, I/O operations and paging. This has made raw code execution speed a
> non-issue for many programmers.
> https://en.wikipedia.org/wiki/Assembly_language

## Chapter 1

* the language is called "symbolic" assembly because symbols are used to represent
    * points in the instruction stream (labels)
    * names of registers
    * locations in memory
    * instruction codes e.g. mov, int etc. are symbols which represent a bit sequence which is the instruction
        * aka "symbolic opcodes"
        * the same memonic e.g. `mov` can be used to generate different instructions (op codes) depending on context
    * instructions to the assembler itself aka
        * assembler direcives
        * pseudo-opcodes or pseudo-ops
* start at the `_start` label:
	* on linux the `_start` label is what the system will transfer control to when it loads the executable
	* `_start` calls the C main function if you wrote you executable in C
* making system calls
	* 32bit asm had `int 0x80
	* x64 added a dedicated (and faster) `syscall` instruction
	* they do NOT use the same sys call numbers!
	* numbers for int 0x80: <http://lxr.free-electrons.com/source/arch/x86/include/asm/unistd_32.h?v=3.0>
	* numbers for syscall:  <http://lxr.free-electrons.com/source/arch/x86/include/asm/unistd_64.h?v=3.0>
* A 64-bit system call is
	1. load the system call number into `rax`
	1. load the params you want to pass into `rdi, rsi, rdx, r10, r8, r9` (in that order)
	1. send the `syscall` opcode to the CPU
	1. CPU switches to kernel mode and the kernel does its thing
	1. Gather return value in `rax`
		* be careful to note that `rcx` and `r11` may have changed during the system call
* So every system call is essentially loading 1-7 numbers into registers and giving up control to the kernel
* Parameters are passed in the order they appear in the function signature of the corresponding C wrapper function.
* You may find syscall functions and their signatures in every Linux API documentation

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


Exercises

* when I built the same trivial program using `main` instead of `_start` and
  using gcc as the linker the binary size went from 1.4K to 8.9K

* Why does shell if statement consider 0 truthy, non-zero falsy
	* In C: 0 = false, non-zero = true
	* Shell if statements: 0 = true, non-zero = false
	* System calls generally return 0 on success so the shell wants to be able to
	test for the success of a program, hence the shell `if` treats 0 as truthy.

* Why do system calls return 0 on success?
	* possible answers
		* 0 is the most cross platform number - you don't have to care about endianness
        * it allows the following coding pattern
            ```c
            error = do_some_syscall(...);
            if (error) {
                // handle error
            }
            // otherwise onwards and upwards
            ```

## Chapter 2

* yasm represents hex numbers using `0x` prefix

* representing negative integers
    * only applies to signed integers
    * if we just used high-order bit as a sign bit it would work but we would need different circuitry to add a positive and negative number and get the correct value
    * we needed a way to represent negative number so they could be added/subtracted/multiplied/divided with positive numbers using the same circuitry as when you have two positive numbers
    * enter _two's compliment_
* to get two's complement representation of a positive intger X
    1. represent the absolute value in binary (ignore sign)
    1. invert all the bits
    1. add 1 to the number

### Floating point representation in 64 bit architectures

* 64 bit architecture supports 3 kinds of floating point number
    * float = 32 bits
    * double = 64 bits
    * long double = 80 bits
* the format of each of the above is the same just with different field lengths

* NOTE: Floating point representation is a sort of "storage format" for a number
    * floats are converted to a standard binary representation before being
      operated on and the results converted back afterwards

* WARNING: in .lst files you will see hex representations of numbers defined in your assembly but the *byte* (not bit) order is reversed
    * The CPU stores the least significant byte first

```
; example .lst file snippet
; notice that the bytes are flipped from how we would expect

     5 00000000 00000000               zero dd 0
     6 00000004 01000000               one dd 1
```

* to do algebraic operation on two floats:
    1. convert them into raw binary numbers
    2. perform the operation
    3. convert result back to float

## Chapter 3: Computer memory

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

