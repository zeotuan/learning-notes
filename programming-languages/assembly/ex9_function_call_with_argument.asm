global main 
extern printf # external symbol printf

section .data
	msg db "The $i", 0x0a, 0x00 # 0x00 is null terminator for c string

main:
	push ebp
	mov ebp, esp
	
	push 123
	push msg
	call printf
	mov eax, 0
	
	mov esp, ebp
	pop ebp
	ret

# 
# gcc -m32 ex10.o -o ex10