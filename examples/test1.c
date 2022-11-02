
int printf(const char *filename, ...);
int o;

void simple_arith(int o) {
	int var = 10, * const ptr, *(*my_fun)(int, _Bool) = printf;
    int x, *z, w = 10;
	for(x = 0;x != 10;x++) {
		label:
		fun();
		printf("value : %d\n",x + *z ^ w);
	}
	goto label;
}