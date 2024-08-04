global _start
_start:
	mov eax, 1 # move 1 into eax register which will be used by system call to determine which system call to execute in this case is system exit call
	mov ebx, 42 # move 42 into ebx register
	sub ebx, 29 # subtract 29 from ebx register


	mov ebx, 123 # ebx = 123
	mov eax, ebx # eax = ebx
	add ebx, ecx # ebx = ebx + ecx
	sub ebx, 4   # ebx = ebx - 4
	mul ebx      # eax = eax * ebx this is a bit different as it always apply multiplication to eax
	div ebx	     # eax = eax / ebx similar to above but it's division

	int 0x80 # perform an interrupt (process will transfer control to an interrupt handler 0x80 which is interrupt handler for system calls)


# build a 32 bit executable and linking format (elf) object file
# nasm -f elf32 ex1.asm -o ex1.o

# build executable from the ex1.o object file -m (x86 architecture)
#  ld -m elf_i386 ex1.o -o ex1