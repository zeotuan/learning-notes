global _start

section .data
	# db is 1 bytes
	addr db "yellow"
	name1 db 0xff
	name2 db 100

	# dw is 2 bytes
	name3 dw 0x1234
	name4 dw 1000

	# dd is 4 bytes
	name5 dd 0x12345678
	name6 dd 100000

	# dword is 4 bytes

section  .text
_start:
	mov [addr], byte 'H' # modify first byte of addr with H
	mov [addr+5], bytes '!' # modify 6th byte of addr with !
	mov eax, 4 # sys_write syscall
	mov ebx, 1 # stdout file descriptor
	mov ecx, addr # bytes to write
	mov edx, 6 # number of bytes to write
	int 0x80

	mov eax, 1 # sys_exit syscall
	mov ebx, 0 # exit code
	int 0x80
 