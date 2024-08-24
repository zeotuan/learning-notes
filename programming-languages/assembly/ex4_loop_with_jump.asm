global _start

section .text
_start:
	mov ebx, 1 	
	mov ecx, 4 # number of  iterations
loop_label:
	add ebx, ebx
	dec ecx # decrement ecx or ecx -= 1
	cmp ecx, 0 #  compare ecx with 0
	jg loop_label # jump to loop_label if ecx > 0
	mov eax, 1
	int 0x80
 