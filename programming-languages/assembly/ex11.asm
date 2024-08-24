global addNum42

addNum42:
	push ebp
	mov ebp, esp

	mov eax, [ebp+8] # get the first argument
	add eax, 42
		
	mov esp, ebp
	pop ebp
	ret