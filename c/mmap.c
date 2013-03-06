#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    struct stat fileinfo;
    int fd;
    int i;
    char *buffer, *ptr;

    fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
	perror("Can't open file for reading");
	return 1;
    }
    fstat(fd, &fileinfo);

    if (fileinfo.st_size > 0) {
        buffer = mmap(0, fileinfo.st_size, PROT_READ, MAP_SHARED, fd, 0);
        if (buffer == MAP_FAILED) {
            close(fd);
            perror("Can't mmap the file");
            return 1;
        }

        ptr = buffer;
        for (i = 0; i < fileinfo.st_size; i++) {
            printf("%c", *ptr);
            ptr++;
        }

        if (munmap(buffer, fileinfo.st_size) == -1) {
            perror("Can't un-mmap the file");
        }
    }

    close(fd);
    return 0;    
}
