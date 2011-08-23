#include <stdio.h>

#define SIZE 1024

int
main(int argc, char **argv)
{
	long g = 1;

	for (;;) {
		printf("g = %ld\n", g);
		g = g * 2;
	}
	return(0);
}
