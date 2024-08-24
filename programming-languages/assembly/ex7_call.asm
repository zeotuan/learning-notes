global _start

_start:
	call func # push the instructer pointer to the stack then perform a jump
	mov eax, 1
	int 0x80

func:
	mov ebx, 42
	pop eax # get the location push onto the stack by call which is the location of the next instruction (mov eax, 1 in this case)
	jmp eax # jump back to the location for the above next instruction
	# ret also do the same thing.
	# but somethings you want to use the stack within the function and if not careful you can overwrite the return address