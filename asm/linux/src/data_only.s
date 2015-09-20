segment .data

; unsigned integers
zero    dd 0
one     dd 1

; notice show signed integers are encoded
negone  dd -1

; notice how floats are encoded
floaty  dd 1.75


; create variables of different sizes
single  db 0xF4
wordy   dw 0xF4
doubly  dd 0xF4
quaddy  dq 0xF4

; reserve 5 contigious bytes and initialize them aka create an array
array_1 db 0x68, 0x65, 0x6C, 0x6C, 0x6F, 0

; reserve 5 contigious bytes and initialize them aka create a string
string_1 db "hello", 0

segment .bss

empty_single  resb 1
empty_wordy   resw 1
empty_doubly  resd 1
empty_quaddy  resq 1

segment .text

global _start
_start:
