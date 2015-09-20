; tells assembler to put the following emitted code into the text section
segment .text

; tells assembler to make the _start symbol visible to the linker
global _start

; _start is the entry point to all linux
_start:
  mov rax, 1  ; 1 is syscall num for exit
  mov rbx, 6  ; the return value
  int 0x80    ; execute the system call
  ; syscall ; the x64 version of int 0x80
