#include <stdio.h>

int
main(int argc, char **argv)
{
	long long a = 1726384394;
	long long b = 67283;

	fprintf(stdout, "%ld, %ld\n", sizeof(a), a % b);
	return(0);
}
