@@ vim: ft=armasm

@@ To assemble:
@@
@@ 	$ gcc first.S

	.data
str: 	.asciz	"Hello world!\n"	@ this is inline comment

	.text
	.globl main

main:
	@ function setup

	@@ This does the following:
	@@
	@@	store multiple, full descending (decrement before), modify sp
        @@
	@@ 1. drecement the value of sp
	@@ 1. store the contents of the lr register at the address pointed to by sp
	@@
	stmfd 	sp!, {lr}	@ push return address onto stack

	@ print the message
	@@ QUESTION: why ldr here an mov later?
	ldr	r0, =str	@ load address of str into r0
	bl	printf		@ call printf

	@ function teardown
	mov	r0, #0		@ put program return code in r0
	ldmfd	sp!, {lr}	@ pop return address from stack
	mov 	pc, lr		@ return and exit program

