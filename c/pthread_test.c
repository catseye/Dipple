#include <stdio.h>
#include <pthread.h>

void proc1(void *arg) {
    int a;
    for (a = 1; a < 1000; a++) {
        printf("value in proc1 is %d\n", a);
    }
}

void proc2(void *arg) {
    int a;
    for (a = 1; a < 1000; a++) {
        printf("value in proc2 is %d\n", a);
    }
}

int main(int argc, char **argv) {
    pthread_t t1, t2;
    int a = 1000, b = 0;

    pthread_create(&t1, NULL, (void *)proc1, (void *)&a);
    pthread_create(&t2, NULL, (void *)proc2, (void *)&b);
    
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    exit(0);
}