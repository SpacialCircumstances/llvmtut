#include <stdio.h>
#include <stdlib.h>

extern void sl_print(long v) {
    printf("%i", v);
}

extern long sl_read() {
    long x;
    if (scanf("%d", &x) != 1) {
        exit(32);
    }
    return x;
}