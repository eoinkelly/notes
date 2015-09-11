# Simple exit program

# linux ELF file format has sections for
# data
# bss
# text

# Mach-o has segments that contain sections

.section __DATA,__data
# segment = __DATA
# section = __data


.section __TEXT,__text
# segment = __TEXT
# section = __text

.globl _main
_main:

  # to do a system call you put the "system call number" in eax and in this
  # case the "exit" system call expects the exit code to be in ebx
  movl $0x2000001, %eax # system call $1 with $0x2000000 offset
  movl $4, %ebx         # set the exit code to be $0
  syscall
