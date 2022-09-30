all: cc

cc: cc.cpp c.tab.cpp c.lex.cpp ast.o ast.h
	g++ ast.o ast.h c.tab.cpp c.lex.cpp cc.cpp -lm -ll -lfl -g -o $@

ast.o: ast.c ast.h
	gcc -c ast.c

c.tab.cpp c.tab.hpp: c.y
	bison -o c.tab.cpp -d c.y

c.lex.cpp: c.l c.tab.hpp
	flex -o c.lex.cpp -l c.l

clean::
	rm -f c.tab.cpp c.tab.hpp c.lex.cpp cc c.output *.o
