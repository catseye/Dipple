#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv)
{
	char *buf;
	size_t size = 1024; // * 1024;
	size_t pages = size / getpagesize();
	int i, j, large = 3000000;

	buf = malloc(size);
	if (mlock(buf, pages) != 0)
		err(1, "mlock(%d)", pages);
	for (i = 1; i < large; i++)
		for (j = 1; j < large; j++)
			;
	if (munlock(buf, pages) != 0)
		err(1, "munlock(%d)", pages);
	free(buf);

	exit(0);
}
