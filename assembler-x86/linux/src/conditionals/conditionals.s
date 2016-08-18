	.file	"conditionals.c"
	.intel_syntax noprefix
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	DWORD PTR [rbp-8], 1
	mov	DWORD PTR [rbp-4], 2
	mov	eax, DWORD PTR [rbp-8]
	cmp	eax, DWORD PTR [rbp-4]
	jne	.L2
	mov	eax, 1
	jmp	.L3
.L2:
	mov	eax, DWORD PTR [rbp-8]
	cmp	eax, DWORD PTR [rbp-4]
	jle	.L4
	mov	eax, 2
	jmp	.L3
.L4:
	mov	eax, DWORD PTR [rbp-8]
	cmp	eax, DWORD PTR [rbp-4]
	jge	.L5
	mov	eax, 3
	jmp	.L3
.L5:
	mov	eax, -1
.L3:
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 4.8.4-2ubuntu1~14.04) 4.8.4"
	.section	.note.GNU-stack,"",@progbits
