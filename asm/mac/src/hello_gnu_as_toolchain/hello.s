# Taken from http: // www.idryman.org/blog/2014/12/02/writing-64-bit-assembly-on-mac-os-x/

# Commands to assemble this
#
# as hello/hello.s -o tmp/hello.o
# ld tmp/hello.o -e _main
# ./a.out

# THIS ASSEMBLES AND LINKS BUT DOES NOT RUN ???

# Mach-o has segments that contain sections

.section __DATA, __data

# segment = __DATA
# section = __data

str:
	.asciz "Hello world!\n"

	.section __TEXT, __text

# segment = __TEXT
# section = __text

.globl _main

_main:

# to do a system call you put the "system call number" in eax and in this
# case the "exit" system call expects the exit code to be in ebx
movl $0x2000001, %eax # system call $1 with $0x2000000 offset
movl $4, %ebx         # set the exit code to be $0
syscall
