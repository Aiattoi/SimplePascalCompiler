#include <stdio.h>

int writeln(int x) {
    printf("%d\n", x);
    return 0;
}

int write(int x) {
    printf("%d", x);
    return 0;
}

int readln(int *x) {
    scanf("%d", x);
    return 0;
}

int writelnS(char x[]) {
    printf("%s\n", x);
    return 0;
}

int writeS(char x[]) {
    printf("%s", x);
    return 0;
}
