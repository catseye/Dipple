#include <stdio.h>
#include <string.h>

int
main(int argc, char **argv)
{
	char *ptr, *word, *arg;

	arg = strdup(argv[1]);
	for (ptr = arg; (word = strsep(&ptr, ",;")) != NULL; ) {
		printf("word: ``%s''\n", word);
		printf("sentence: ``%s''\n", ptr);
		printf("arg: ``%s''\n", arg);
		printf("argv[1]: ``%s''\n\n", argv[1]);
	}
	free(arg);
	
	return(0);
}
