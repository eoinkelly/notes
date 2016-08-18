@@ vim: ft=armasm
/*
To assemble:

	$ gcc first.S
*/

	.data
str: 	.asciz 	"Hello world!\n" 	@ this is inline comment

	.text
	.globl main

main:
	@ stmfd 	sp!, [lr]
	ldr	r0, =str
	bl	printf
	mov	r0, #0
	// ldmfd	sp!, [lr]
	mov 	pc, lr

