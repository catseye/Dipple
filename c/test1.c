#include <stdio.h>

#define SIZE 1024

int
main(int argc, char **argv)
{
	char string[SIZE + 1];
	int i;

        srand(time(0));
	for (;;) {
		for (i = 0; i < SIZE; i++)
			string[i] = (random() % 26) + 'A';
		string[i] = '\0';
		printf("%s\n", string);
		sleep(1);
	}
	return(0);
}
