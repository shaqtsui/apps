	.file	"simple.c"
	.text
	.globl	simple
	.type	simple, @function
simple:
.LFB0:
	.cfi_startproc
	movl	%esi, %eax
	addl	(%rdi), %eax
	movl	%eax, (%rdi)
	ret
	.cfi_endproc
.LFE0:
	.size	simple, .-simple
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.4) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
