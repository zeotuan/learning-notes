global _start

_start:
	call func # push the instructer pointer to the stack then perform a jump
	mov eax, 1
	mov ebx, 0
	int 0x80

func:
	# the prologue of a function
	push ebp # push original base pointer to the stack to allow nested function calls
	mov ebp, esp # ebp is the base pointer register which is used to hold the location of the top of stack when the function is called

	# function body
	sub esp, 2
	mov [esp], byte 'H'
	mov [esp+1], byte 'i'
	mov eax, 4
	mov ebx, 1
	mov ecx, esp
	mov edx, 2
	int 0x80

	# the epilogue of a function
	mov esp, ebp # restore the stack pointer to the top of the stack
	pop ebp
	ret