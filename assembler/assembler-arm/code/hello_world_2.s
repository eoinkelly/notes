@@ vim: ft=armasm

@@ This version does not need the C stdlib
@@ * it provides its own _start function rather than using the one in stdlib
@@
@@ To assemble:
@@ 	$ as -o hello2.o -aghmls=hello.lst hello_world_2.s
@@ 	$ ld -o hello2 hello2.o

		.data
msg:		.ascii 	"Hello world\n"
len		= . - msg

		.text
		.globl _start

_start:
		mov r0, #1 	@@ fd = stdout
		ldr r1, =msg	@@ r1 -> message
		ldr r2, =len	@@ r2 -> message length
		mov r7, #4	@@ write syscall is 4
		swi #0		@@ invoke syscall

		mov r0, #0 	@@ r0 = the exit code
		mov r7, #1 	@@ exit is syscall 1
		swi 	#0	@@ invoke syscall
