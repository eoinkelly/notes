	; **********************************
	; A minimal assembly program
	; **********************************

	; this is *symbolic* assmebly code because:
	; 1. the instructions are refered to by symbols e.g. mov, int
	; 2. register names and memory locations are refered to by symbols
	; 3. labels (symbols) are used to mark points in the instruction stream for
	; branching.

	;       tells the assembler to put the following emitted code into the text
	;       section of the executable.
	segment .text

	;      tells assembler to make the "_start" symbol visible to the linker
	global _start

	; _start is a label and by convention is the entry point to all linux
	; executables

_start:
	; mov      rax, 1; 1 is syscall num for exit
	; mov      rbx, 6; the return value
	; int 0x80; execute the system call

	;        system call numbers for syscall:  http:                // lxr.free-electrons.com/source/arch/x86/include/asm/unistd_64.h?v=3.0
	;        system call numbers for int 0x80: http:                // lxr.free-electrons.com/source/arch/x86/include/asm/unistd_32.h?v=3.0
	mov      rax, 60; 60 is syscall num for exit
	mov      rdi, 6; the return value
	syscall; x64 added a dedicated (and faster) syscall instruction
