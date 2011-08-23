#include <stdio.h>

int
main(int argc, char **argv)
{
	FILE *f;
	char line[80];

	if ((f = popen("script -q /dev/null ./test1", "r")) == NULL)
		exit(64);

	while (!feof(f)) {
		fgets(line, 79, f);
		if (feof(f))
			break;
		printf("got: %s", line);
		fflush(stdout);
	}

	pclose(f);
	return(0);
}
