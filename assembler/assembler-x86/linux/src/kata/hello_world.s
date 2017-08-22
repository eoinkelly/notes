; linux assembler


segment .data

msg	dd	123

segment .text

global _start

_start:
	mov rax, 0xbe
