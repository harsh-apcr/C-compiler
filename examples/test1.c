
int printf(const char *filename, ...);
int o;

void simple_arith(int o) {
    int x, *z, w = 10;
	for(x = 0;x != 10;x++) {
		label:
		printf("value : %d\n",x + *z ^ w);
	}
	goto label;
}