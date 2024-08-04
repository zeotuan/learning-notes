global _start

section .text
_start:
	mov ecx, 99
	mov ebx, 42 # exit status is 42
	mov eax, 1  # sys_exit system call
	cmp ecx, 100 # compare ecx with 100
	jl  skip # jump to "skip" label
	mov ebx, 13 # this move will not be executed
skip:
	int 0x80



# je A, B - jump if equal
# jne A, B - jump if not equal
# jg A, B - jump if greater
# jge A, B - jump if greater or equal
# jl A, B - jump if less
# jle A, B - jump if less or equal