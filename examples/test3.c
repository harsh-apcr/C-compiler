
int foo() {
    my_foo: {
        int x, y;
        for(int x=0;x<10;x--) { // double declaration of x
            y += x;
            y *= y;
            goto my_foo;
            continue;break;return 1;
        }
    }
}

void main() {
    bar:
    printf("hello-world\n");
    for(;;) {}
    return;
}