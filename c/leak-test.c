#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <leak_detector.h>

char *mstrdup(char *s)
{
	char *q;
	
	q = malloc(strlen(s) + 1);
	strcpy(q, s);
	return(q);
}

void tube(void)
{
	int i;
	char *s;
	s = mstrdup("This ol' string. !^^(@^6863986298");
	for (i = 0; i < strlen(s); i++)
		s[i] = toupper(s[i]);
	printf("%s\n", s);
}

int main(int argc, char *argv[])
{
	GC_find_leak = 1;
	tube();
	CHECK_LEAKS();
	return(0);
}
