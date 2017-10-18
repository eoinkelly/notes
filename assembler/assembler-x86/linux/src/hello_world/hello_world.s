; x86_64 assembler targeting linux
section     .data		; Tell the compiler to put this in the "data" section

msg     db  'Hello, world!',0x0A
len     equ $ - msg
; len     equ 14

section     .text		; Tell the compiler to put the following instructions in the "code" section
global      _start		; tell the linker that we are defining the "_start" symbol

_start:				; label this point in the instruction stream
	; printf("Hello, "World");
	mov     rdx, len        ;message length
	mov     rsi, msg        ;message to write
	mov     rdi, 1          ;file descriptor (stdout)
	mov     rax, 1          ;system call number (sys_write)
	syscall

	; return(0);
	mov      rax, 60	; 60 is syscall num for exit
	mov      rdi, 0		; the return value
	syscall			; x64 added a dedicated (and faster) syscall instruction
