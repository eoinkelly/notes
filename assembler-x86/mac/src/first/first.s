	;      linux uses '_start' as entry point
	;      mac uses 'start' as entry point
	global start

	;        mac requires you to have a .data section with something in it or you
	;        see a `dyld: no writable segment` error when you execute the binary.
	;        See stackoverflow.com/a/9905758/356025
	section  .data
	bullshit db 0

	;       linux uses 'segment' to delimit parts of the binary
	;       mac uses 'section' to delimit parts of the binary
	section .text

	; opensource.apple.com // source/xnu/xnu-1504.3.12/bsd/kern/syscalls.master

start:
	mov rax, 0x2000001; exit
	mov rdi, 3
	syscall
