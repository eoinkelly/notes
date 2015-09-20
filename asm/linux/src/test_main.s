segment .text
global main

; this program uses main as entry point so needs to be linked with gcc which
; will provide its own _start
main:
  mov eax, 1  ; 1 is syscall num for exit
  mov ebx, 6  ; the return value
  int 0x80    ; execute the system call
