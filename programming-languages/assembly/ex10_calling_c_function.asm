global _start

_start:
	push 1234
	call times2 # push the instructer pointer to the stack then perform a jump
	mov ebx, eax
	mov eax, 1
	int 0x80

times2:
	# the prologue of a function
	push ebp # push original base pointer to the stack to allow nested function calls
	mov ebp, esp # ebp is the base pointer register which is used to hold the location of the top of stack when the function is called

	# function body
	mov eax, [ebp+8] # get the first argument of the function, need to increase the offset by 4 bytes because when call is executed, the return address is also pushed onto the stack 
	add eax, eax # multiply the first argument by 2
	
	# the epilogue of a function
	mov esp, ebp # restore the stack pointer to the top of the stack
	pop ebp
	ret