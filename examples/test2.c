
int foo() {
    bar();
}

int bar() {
    int x = foobar("hello world", (110 - 56) * 254 % 100 / 200 ^ 0xC0FFEE);
}

int foobar(const char *str, int x, ...) {
    printf(str);
    int y = (x & 0xC8371) | 0x1239813;
    int *arr;
    int z = arr[y];
}