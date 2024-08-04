global _start

section  .text
_start:
	# pushing 4 bytes int to the stack 
	push 1234
	push 5678
	# same implementation to push number to the stack
	sub esp, 4
	mov [esp], dword 357

	# pop operation
	pop eax # move number top of stack (357) into eax register 
	# manually implement same pop operation
	mov eax, dword [esp] # move number top of stack (5678) into eax register
	add esp, 4 # move stack pointer to point to next number