#include <stdio.h>

int
function(int arg1, char *arg2, char arg3, double arg4)
{
    printf("-----:\n");
    printf("arg1: %d\n", arg1);
    printf("arg2: %s\n", arg2);
    printf("arg3: %c\n", arg3);
    printf("arg4: %9.8g\n\n", arg4);
    
    return 0;
}

typedef char byte;
byte ary[13];

struct foo { byte data[24]; } fee;
typedef int (*foo_fun)(struct foo);

struct bar {
    int arg1;
    char *arg2;
    char arg3;
    double arg4;
} bee;
typedef int (*bar_fun)(struct bar);

int
fun2(struct foo f) {
    printf("arg1: %d\n", f.data[0]);
    return 0;
}

inline void
poke_int(void *address, int *offset, int value) {
    *(int *)((byte *)address + *offset) = value;
    *offset += sizeof(int);
}

inline void
poke_ptr(void *address, int *offset, void *value) {
    *(byte **)((byte *)address + *offset) = value;
    *offset += sizeof(void *);
}

inline void
poke_char(void *address, int *offset, char value) {
    *(char *)((byte *)address + *offset) = value;
    *offset += sizeof(int);
}

inline void
poke_double(void *address, int *offset, double value) {
    *(double *)((byte *)address + *offset) = value;
    *offset += sizeof(double);
}

int
main(int argc, char **argv)
{
    printf("sizeof ary=%d\n", sizeof(ary));
    printf("sizeof foo=%d\n", sizeof(struct foo));
    //fun2(fee);
    //(int *)(fee.data[0]) = 55;
    //(char **)(fee.data[0]) = "hello";
    //(char **)(fee.data[sizeof(int)]) = "hello";
    //(char *)(fee.data[sizeof(int) + sizeof(char *)]) = 'k';
    //(double *)(fee.data[sizeof(int) + sizeof(char *) + sizeof(double)]) = 178.213;
    //((foo_fun)function)(fee);

    bee.arg1 = 55;
    bee.arg2 = "hello";
    bee.arg3 = 'k';
    bee.arg4 = 178.213;

    ((bar_fun)function)(bee);

    {
    int offset = 0;
    poke_int(&fee, &offset, 66);
    poke_ptr(&fee, &offset, "goodbye");
    poke_char(&fee, &offset, 'j');
    poke_double(&fee, &offset, 91.3171);
    }

    ((foo_fun)function)(fee);
}
