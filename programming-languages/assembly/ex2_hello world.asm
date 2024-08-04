global _start

section .data # inline data into the program which can be referenced by the program later by name
  msg db "Hello, World!", 0x0a # 0x0a is newline character
  len equ $ - msg # $ is a special symbol that represents the current address of the program counter


section .text # code section
_start:
	mov eax, 4 #  sys_write system call 
	mov ebx, 1 #  stdout file descriptor
	mov ecx, msg # bytes to write
	mov edx, len # number of bytes to write
	int 0x80  # perform system call

	mov eax, 1 
	mov ebx, 0
	int 0x80