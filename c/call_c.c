#include <stdio.h>

int
function(int arg1, char *arg2, char arg3, double arg4)
{
	printf("arg1: %d\n", arg1);
	/*
	printf("arg2: %s\n", arg2);
	printf("arg3: %c\n", arg3);
	printf("arg4: %9.8g\n", arg4);
   */
	
	return arg1 / 2;
}

typedef char byte;
typedef int (*arfun)(byte[]);

int
main(int argc, char **argv)
{
	byte *args;
	int i;

	args = (byte *)malloc(sizeof(int) + sizeof(char *) + sizeof(char) + sizeof(double));
	
	args[0] = 0;
	args[1] = 0;
	args[2] = 0;
	args[3] = 0;

	/*
	(int *)(args[0]) = 55;
	(char **)(args[sizeof(int)]) = "hello";
	(char *)(args[sizeof(int) + sizeof(char *)]) = 'k';
	(double *)(args[sizeof(int) + sizeof(char *) + sizeof(double)]) = 178.213;
	*/

	i = ((arfun)function)(args);
	free(args);

	printf("return: %d\n", i);
	return(0);
}
