#include "leak_detector.h"

main() {
    int *p[10];
    int i;
    GC_find_leak = 1;
    for (i = 0; i < 10; ++i) {
        p[i] = malloc(sizeof(int)+i);
    }
    for (i = 1; i < 10; ++i) {
        free(p[i]);
    }
    for (i = 0; i < 9; ++i) {
        p[i] = malloc(sizeof(int)+i);
    }
    CHECK_LEAKS();
}
