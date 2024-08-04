#include <stdio.h>
#include "add42.h"

// gcc -m32 ex11.o main.c -o ex11c
int main()
{
	int result;
	result = add42(10);
	printf("Result: %i\n", result);
	return 0;
}