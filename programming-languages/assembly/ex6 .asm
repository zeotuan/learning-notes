global _start

section  .text
_start:
	sub esp, 4 # allocate 4 bytes on the stack
	mov [esp], bytes 'H'
	mov [esp+1], bytes 'e'
	mov [esp+2], bytes 'l'
	mov [esp+3], bytes 'l'
	mov [esp+4], bytes 'o'
	mov [esp+5], bytes '!'
	mov eax, 4 # sys_write syscall
	mov ebx, 1 # stdout file descriptor
	mov ecx, esp # bytes to write
	mov edx, 6 # number of bytes to write
	int 0x80

	mov eax, 1 # sys_exit syscall
	mov abx, 0 # exit code
	int 0x80